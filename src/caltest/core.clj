;; N.B. This program currently only extracts start times. Adding
;; support for block events shouldn't be hard, but there are some
;; severe limitations in org-mode's support for block events.

;;  Usage: <binary> <path-to-icalendar-file>

(ns caltest.core
  (:use clj-time.core
        clj-time.format
        clj-time.coerce
        [clojure.string :only (join)])
  (:import net.fortuna.ical4j.data.CalendarBuilder
           java.io.StringReader
           net.fortuna.ical4j.model.Calendar))

;; Begin iCalendar parsing code.
(defn- parse-date-time
  "Parse a date/time string to a DateTime object."
  [date]
  (let [format (formatter (default-time-zone)
                          ;; Add more time formats as needed!
                          ;; See huge comment section after the main code
                          ;; for reference to jodatime's syntax.
                          ;; Very important: all literals must be quoted!
                          "yyyyMMdd'T'HHmmss'Z'"
                          "yyyyMMdd"
                          "yyyyMMdd'T'HHmmss"
                          "yyyyMMdd'T'HHmmss'Z'Z")]
    (parse format date)))


(defn- extract-property-keyword
  "Reduce a property to a usable keyword for a hash-map."
  [property]
  (let [property-name (.. property getName toLowerCase)]
    (keyword property-name)))

(defn- is-time-stamp?
  "Returns true if a given key represents a timestamp."
  [key]
  (let [time-stamp-keys #{:dtstart, :dtstamp, :dtend}]
    (contains? time-stamp-keys key)))

(defn- extract-property-value
  "Handle a property value (convert timestamps to time objects etc."
  [property]
  (let [value (.getValue property)
        keyword (extract-property-keyword property)]
    (if (is-time-stamp? keyword)
      (parse-date-time value)
      value)))

(defn- properties-to-hash
  "Take a list of properties of a calendar and convert it to a hash map."
  [properties]
  (let [name-values
        (flatten (map #(vector (extract-property-keyword %) (extract-property-value %))
                      properties))]
    (apply hash-map name-values)))

(defn- event-to-hash
  "Take an event and convert it to a hash map."
  [event]
  (let [event-type (.getName event) ;; should be VEVENT
        event-properties-seq (iterator-seq (.. event getProperties iterator))
        event-properties (properties-to-hash event-properties-seq)]
    (assoc event-properties :event-type event-type)))

(defn- events-to-hash
  "Take a iterator-seq of events of a calendar and convert it to a hash map."
  [events]
  (map event-to-hash events))

(defn ical-to-hash
  "Convert an iCalendar string to a hash-map."
  [cal]
  (let [builder (CalendarBuilder.)
        testsr (StringReader. cal)
        testcal (.build builder testsr)
        cal-events-seq  (iterator-seq (.. testcal getComponents iterator))
        cal-prop-seq (iterator-seq (.. testcal getProperties iterator))
        cal-properties (properties-to-hash cal-prop-seq)
        events (events-to-hash cal-events-seq)]
    (hash-map :events events
              :properties cal-properties)))

;; End of iCalendar parsing functions.

;; Begin org-mode generation functions.
(defn- date-to-org-timestamp
  "Convert a date/time object to an org-mode ISO timestamp, with optionally supplied brackets/delimiters."
  ([time start-delimiter end-delimiter]
     (let [date-format (formatter "yyyy-MM-dd E HH:mm")
           formatted-string (unparse date-format time)]
       (str start-delimiter formatted-string end-delimiter)))
  ([time]
     (date-to-org-timestamp time "" "")))

(defn org-passive-timestamp
  "Make a passive org-mode timestamp from a date object."
  [time]
  (date-to-org-timestamp time "[" "]"))

(defn org-active-timestamp
  "Make an active org-mode timestamp from a date object."
  [time]
  (date-to-org-timestamp time "<" ">"))

(defn org-heading
  "Make an org-mode heading with optional contents."
  [title & contents]
  (let [flat-contents (join "\n" contents)]
    (format "* %s\n %s\n " title flat-contents)))

(defn event-to-org
  "Transform a parsed event to a org-mode heading with timestamp."
  [event]
  (let [event-title (:summary event)
        event-timestamp (org-active-timestamp (:dtstart event))]
    (org-heading event-title event-timestamp)))

;; End of org-mode generation functions.

(defn -main [calfile & rest]
  (let [ics-data (slurp calfile)
        parsed-calendar (ical-to-hash ics-data)
        events (:events parsed-calendar)
        org-events (map event-to-org events)]
    (doseq [org-heading org-events]
      (println org-heading))))

;; From jodatime: http://kickjava.com/src/org/joda/time/format/DateTimeFormat.java.htm
;; * Symbol Meaning Presentation Examples
;; 59  * ------ ------- ------------ -------
;; 60  * G era text AD
;; 61  * C century of era (&gt;=0) number 20
;; 62  * Y year of era (&gt;=0) year 1996
;; 63  *
;; 64  * x weekyear year 1996
;; 65  * w week of weekyear number 27
;; 66  * e day of week number 2
;; 67  * E day of week text Tuesday; Tue
;; 68  *
;; 69  * y year year 1996
;; 70  * D day of year number 189
;; 71  * M month of year month July; Jul; 07
;; 72  * d day of month number 10
;; 73  *
;; 74  * a halfday of day text PM
;; 75  * K hour of halfday (0~11) number 0
;; 76  * h clockhour of halfday (1~12) number 12
;; 77  *
;; 78  * H hour of day (0~23) number 0
;; 79  * k clockhour of day (1~24) number 24
;; 80  * m minute of hour number 30
;; 81  * s second of minute number 55
;; 82  * S fraction of second number 978
;; 83  *
;; 84  * z time zone text Pacific Standard Time; PST
;; 85  * Z time zone offset/id zone -0800; -08:00; America/Los_Angeles
;; 86  *
;; 87  * ' escape for text delimiter
;; 88  * '' single quote literal '