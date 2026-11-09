(in-package #:local-time.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time::define-timezone eastern-tz
      (merge-pathnames #p"EST5EDT" local-time::*default-timezone-repository-path*))
  (local-time::define-timezone cet-tz
      (merge-pathnames #p"Europe/Amsterdam" local-time::*default-timezone-repository-path*))
  (local-time::define-timezone utc-leaps
      (merge-pathnames #p"right/UTC" local-time::*default-timezone-repository-path*))
  (local-time::define-timezone anchorage
      (merge-pathnames #p"America/Anchorage" local-time::*default-timezone-repository-path*)
    :load t)
  (local-time::define-timezone ist
      (merge-pathnames #p"Asia/Kolkata" local-time::*default-timezone-repository-path*))
  (local-time::define-timezone portugal
      (merge-pathnames #p"Portugal" local-time::*default-timezone-repository-path*))
  (local-time::define-timezone eet
      (merge-pathnames #p"EET" local-time::*default-timezone-repository-path*))
  )

(deftest offset/type-anchorage ()
  (every (lambda (subzone)
           (let ((offset (local-time::subzone-offset subzone)))
             (is (typep offset 'local-time::timezone-offset)
                 "offset ~a not of type timezone-offset"
                 offset)))
         (local-time::timezone-subzones anchorage)))

(deftest transition-position/correct-position ()
  (let ((cases '((0 #(1 2 3 4 5) -1)
                 (1 #(1 2 3 4 5) 0)
                 (2 #(1 2 3 4 5) 1)
                 (3 #(1 2 3 4 5) 2)
                 (4 #(1 2 3 4 5) 3)
                 (5 #(1 2 3 4 5) 4)
                 (1 #(1 3 5) 0)
                 (2 #(1 3 5) 0)
                 (3 #(1 3 5) 1)
                 (4 #(1 3 5) 1)
                 (5 #(1 3 5) 2)
                 (6 #(1 3 5) 2)
                 (1 #(1 3 5 7) 0)
                 (2 #(1 3 5 7) 0)
                 (3 #(1 3 5 7) 1)
                 (4 #(1 3 5 7) 1)
                 (5 #(1 3 5 7) 2)
                 (6 #(1 3 5 7) 2)
                 (7 #(1 3 5 7) 3)
                 (8 #(1 3 5 7) 3)
                 )))
    (dolist (case cases)
      (destructuring-bind (needle haystack want)
          case
        (let ((got (local-time::transition-position needle (coerce haystack '(simple-array (signed-byte 32) (*))))))
          (is (= got want)
              "(transition-position ~a ~a) got ~a, want ~a"
              needle haystack got want))))))

(defparameter *dst-test-cases*
  `((,eastern-tz
     ;; Spring forward
     ((2008 3 9 6 58) (2008 3 9 1 58))
     ((2008 3 9 6 59) (2008 3 9 1 59))
     ((2008 3 9 7  0) (2008 3 9 3  0))
     ((2008 3 9 7  1) (2008 3 9 3  1))
     ;; Fall back
     ((2008 11 2 5 59) (2008 11 2 1 59)
      ;; 01:59 EST is ambiguous on that day, the
      ;; encoding prefers the later absolute time.
      (2008 11 2 6 59))
     ((2008 11 2 6  0) (2008 11 2 1  0))
     ((2008 11 2 6  1) (2008 11 2 1  1))
     ((2008 11 2 6 59) (2008 11 2 1  59)))
    (,cet-tz
     ;; Spring forward
     ((2023 3 26 0 59) (2023 3 26 1 59))
     ((2023 3 26 1  0) (2023 3 26 3  0))
     ((2023 3 26 1  1) (2023 3 26 3  1))))
  "A list of expressions (tz test-case*).
Each test-case consists of two or three time expressions:
  (T-UTC T-TZ [T-UTC'])
Encoding T-UTC in UTC and decoding the result in TZ should yield T-TZ.
Ecnoding T-TZ in TZ and decoding the result in UTC should yield T-UTC;
except for cases of ambiguous wall times during a DST fall back, where T-UTC'
is expected instead.")

(deftest test/timezone/decode-timestamp-dst ()
  ;; Testing DST calculation with a known timezone
  (dolist (tz-test-cases *dst-test-cases*)
    (destructuring-bind (tz . test-cases) tz-test-cases
      (dolist (test-case (cdr test-cases))
        (is (equal
             (reverse
              (subseq
               (multiple-value-list
                (let ((timestamp
                       (apply 'local-time:encode-timestamp
                              `(0 0 ,@(reverse (first test-case)) :offset 0))))
                  (local-time:decode-timestamp timestamp :timezone tz)))
               2 7))                 ;min, ..., year and reversed year, ..., min
             (second test-case)))))))

(deftest test/timezone/decode-universal-dst ()
  ;; Testing DST calculation with a known timezone
  (dolist (tz-test-cases *dst-test-cases*)
    (destructuring-bind (tz . test-cases) tz-test-cases
      (dolist (test-case (cdr test-cases))
        (is (equal
             (reverse
              (subseq
               (multiple-value-list
                (let ((universal
                       (apply #'encode-universal-time
                              `(0 ,@(reverse (first test-case)) 0))))
                  (local-time:decode-universal-time-with-tz universal
                                                            :timezone tz)))
               1 6))                 ;min, ..., year and reversed year, ..., min
             (second test-case)))))))

(deftest test/timezone/formatting ()
  ;; Zone Asia/Kolkata has positive fractional hour offset;
  ;; zone Portugal has a negative fractional hour offset (in 1901).
  (is (equal (format-timestring nil (encode-timestamp 0 0 0 0 1 1 2000 :offset 0)
                                :timezone ist)
             "2000-01-01T05:30:00.000000+05:30"))
  (is (equal (format-timestring nil (encode-timestamp 0 0 0 0 5 12 1901 :offset 0)
                                :timezone portugal)
             "1901-12-04T23:23:15.000000-00:37")))

(deftest test/timezone/encode-timestamp-dst ()
  ;; Testing DST calculation with a known timezone, encoded in the timezone
  (dolist (tz-test-cases *dst-test-cases*)
    (destructuring-bind (tz . test-cases) tz-test-cases
      (dolist (test-case test-cases)
        (is (equal
             (reverse
              (subseq
               (multiple-value-list
                (let ((timestamp
                       (apply 'local-time:encode-timestamp
                              `(0 0 ,@(reverse (second test-case)) :timezone ,tz))))
                  (local-time:decode-timestamp timestamp :offset 0)))
               2 7))
             ;; Allow for ambiguous local times
             (or (third test-case) (first test-case))))))))

(deftest test/timezone/encode-universal-dst ()
  ;; Testing DST calculation with a known timezone, encoded in the timezone
  (dolist (tz-test-cases *dst-test-cases*)
    (destructuring-bind (tz . test-cases) tz-test-cases
      (dolist (test-case test-cases)
        (is (equal
             (reverse
              (subseq
               (multiple-value-list
                (let ((universal
                       (apply 'local-time:encode-universal-time-with-tz
                              `(0 ,@(reverse (second test-case)) :timezone ,tz))))
                  (decode-universal-time universal 0)))
               1 6))
             ;; Allow for ambiguous local times
             (or (third test-case) (first test-case))))))))

(deftest test/timezone/universal-values ()
  ;; Minimal consistency for day-of-week daylight-p, offset, abbrevation
  (let ((march-utc (encode-universal-time 0 0 0 1 3 2023 0))
        (march-decode '(0 0 1 1 3 2023 2 nil -1 "CET"))
        (june-utc (encode-universal-time 0 0 0 1 6 2023 0))
        (june-decode '(0 0 2 1 6 2023 3 t -2 "CEST")))
    (is (equal (multiple-value-list
                (local-time:decode-universal-time-with-tz march-utc
                                                          :timezone cet-tz))
               march-decode))
    (is (equal (apply #'local-time:encode-universal-time-with-tz
                      `(,@(subseq march-decode 0 6) :timezone ,cet-tz))
               march-utc))
    (is (equal (multiple-value-list
                (local-time:decode-universal-time-with-tz june-utc
                                                          :timezone cet-tz))
               june-decode))
    (is (equal (apply #'local-time:encode-universal-time-with-tz
                      `(,@(subseq june-decode 0 6) :timezone ,cet-tz))
               june-utc))))

(deftest test/timezone/strict-validity ()
  ;; EET is Athens, whose first transition is in 1895.  Before it the zone
  ;; keeps LMT (Local mean time, +1:34:52), which is why the minutes are at
  ;; an odd value.
  (flet ((_eet-to-utc (year)
           (multiple-value-list
            (local-time:decode-timestamp
             (local-time:encode-timestamp 0 0 0 0 1 4 year :timezone eet)
             :offset 0)))
         (_encode-eet (year)
           (local-time:encode-timestamp 0 0 0 0 1 4 year :timezone eet)))
    (let ((local-time::*strict-first-subzone-validity* nil))
      (is (equal (reverse (subseq (_eet-to-utc 1890) 2 7))
                 '(1890 3 31 22 25))))
    (let ((local-time::*strict-first-subzone-validity* t))
      (signals simple-error (_encode-eet 1890))
      ;; Inside the table, which the 64-bit block extends back to 1895.
      (is (_encode-eet 1900)))))

(deftest test/timezone/posix-rule-parsing ()
  (flet ((parse (string)
           (let ((rule (local-time::%parse-posix-tz string)))
             (when rule
               (list (local-time::subzone-abbrev (local-time::tz-rule-std rule))
                     (local-time::subzone-offset (local-time::tz-rule-std rule))
                     (let ((dst (local-time::tz-rule-dst rule)))
                       (when dst
                         (list (local-time::subzone-abbrev dst)
                               (local-time::subzone-offset dst))))
                     (local-time::tz-rule-start rule)
                     (local-time::tz-rule-start-time rule)
                     (local-time::tz-rule-end rule)
                     (local-time::tz-rule-end-time rule))))))
    (is (equal (parse "EST5EDT,M3.2.0,M11.1.0")
               '("EST" -18000 ("EDT" -14400) (:month 3 2 0) 7200 (:month 11 1 0) 7200)))
    (is (equal (parse "IST-2IDT,M3.4.4/26,M10.5.0")
               '("IST" 7200 ("IDT" 10800) (:month 3 4 4) 93600 (:month 10 5 0) 7200)))
    (is (equal (parse "AEST-10AEDT,M10.1.0,M4.1.0/3")
               '("AEST" 36000 ("AEDT" 39600) (:month 10 1 0) 7200 (:month 4 1 0) 10800)))
    (is (equal (parse "<-03>3<-02>,M3.5.0/-2,M10.5.0/-1")
               '("-03" -10800 ("-02" -7200) (:month 3 5 0) -7200 (:month 10 5 0) -3600)))
    (is (equal (parse "IST-5:30") '("IST" 19800 nil nil 7200 nil 7200)))
    (is (equal (parse "EST5EDT,0/0,J365/25")
               '("EST" -18000 ("EDT" -14400) (:zero-based 0) 0 (:julian 365) 90000)))
    (is (null (parse "")))
    (is (null (parse "E5")))
    (is (null (parse "EST5EDT,M13.1.0,M11.1.0")))
    (is (null (parse "EST5EDT,M3.2.0")))))

(defun %offset-in (zone-name year month day &optional (hour 12))
  (multiple-value-list
   (timestamp-subtimezone
    (encode-timestamp 0 0 0 hour day month year :timezone +utc-zone+)
    (find-timezone-by-location-name zone-name))))

(deftest test/timezone/rule-after-last-transition ()
  ;; tzdata enumerates transitions only to 2037; past that the footer rule
  ;; applies, so daylight saving must keep alternating -- in both directions.
  (is (equal (subseq (%offset-in "America/New_York" 2039 6 15) 0 2) '(-14400 t)))
  (is (equal (subseq (%offset-in "America/New_York" 2039 12 15) 0 2) '(-18000 nil)))
  (is (equal (subseq (%offset-in "Australia/Sydney" 2039 6 15) 0 2) '(36000 nil)))
  (is (equal (subseq (%offset-in "Australia/Sydney" 2039 12 15) 0 2) '(39600 t)))
  (is (equal (%offset-in "Europe/London" 2400 7 1) '(3600 t "BST")))
  ;; US daylight time in 2040 starts 11 March at 02:00 EST, 07:00 UTC.
  (is (equal (subseq (%offset-in "America/New_York" 2040 3 11 6) 0 2) '(-18000 nil)))
  (is (equal (subseq (%offset-in "America/New_York" 2040 3 11 7) 0 2) '(-14400 t)))
  ;; Jerusalem's rule is M3.4.4/26: 26:00 on the fourth Thursday of March,
  ;; so 02:00 IST on Friday 29 March 2041, which is 00:00 UTC.
  (is (equal (subseq (%offset-in "Asia/Jerusalem" 2041 3 28 23) 0 2) '(7200 nil)))
  (is (equal (subseq (%offset-in "Asia/Jerusalem" 2041 3 29 0) 0 2) '(10800 t)))
  ;; Encoding a local time goes through the same rule.
  (is (= (timestamp-to-unix
          (encode-timestamp 0 0 0 12 1 7 2050
                            :timezone (find-timezone-by-location-name "America/New_York")))
         (timestamp-to-unix
          (encode-timestamp 0 0 0 16 1 7 2050 :timezone +utc-zone+)))))

(defun %rule-only-subzone (rule unix-time)
  "The subzone RULE alone puts in force at UNIX-TIME, ignoring any table."
  (let* ((year (local-time::%year-from-days (floor unix-time 86400)))
         (entries (loop for y from (1- year) to (1+ year)
                        append (local-time::%tz-rule-transitions rule y))))
    (cdr (car (last (remove-if (lambda (entry) (> (car entry) unix-time))
                               entries))))))

(defun %rule-zones ()
  "Zones with a daylight-saving rule and a table, excluding the right/ zones,
whose tables count leap seconds while their rules are in civil time."
  (loop for zone being the hash-values of local-time::*location-name->timezone*
        for rule = (local-time::timezone-rule zone)
        when (and rule
                  (local-time::tz-rule-dst rule)
                  (null (local-time::timezone-leap-seconds zone))
                  (>= (length (local-time::timezone-transitions zone)) 2))
          collect zone))

(deftest test/timezone/rule-continues-from-table ()
  ;; RFC 8536 3.3: the footer rule must agree with the state the last
  ;; transition leaves in force.
  (dolist (zone (%rule-zones))
    (let* ((transitions (local-time::timezone-transitions zone))
           (last (1- (length transitions)))
           (table (svref (local-time::timezone-subzones zone)
                         (aref (local-time::timezone-indexes zone) last)))
           (rule (%rule-only-subzone (local-time::timezone-rule zone)
                                     (aref transitions last))))
      ;; Compared by value: zic can emit identical types more than once.
      (is (and rule
               (equal (local-time::subzone-abbrev table) (local-time::subzone-abbrev rule))
               (eql (local-time::subzone-offset table) (local-time::subzone-offset rule))
               (eq (local-time::subzone-daylight-p table) (local-time::subzone-daylight-p rule)))
          "~a: the table leaves ~a in force but the rule gives ~a"
          (local-time::timezone-name zone)
          (local-time::subzone-abbrev table)
          (and rule (local-time::subzone-abbrev rule))))))

(deftest test/timezone/rule-agrees-with-table ()
  ;; Where the table stops at tzdata's usual 2037 cutoff, its final year was
  ;; generated by zic from the same rule, so they must agree exactly.  (A few
  ;; zones, like Asia/Gaza, enumerate irregular transitions beyond that.)
  (dolist (zone (%rule-zones))
    (let* ((rule (local-time::timezone-rule zone))
           (transitions (local-time::timezone-transitions zone))
           (count (length transitions)))
      (when (= 2037 (local-time::%year-from-days
                     (floor (aref transitions (1- count)) 86400)))
        (loop for idx from (- count 2) below count
              for time = (aref transitions idx)
              for year = (local-time::%year-from-days (floor time 86400))
              do (is (member time (local-time::%tz-rule-transitions rule year)
                             :key #'car)
                     "~a: table transition ~a is not produced by its rule"
                     (local-time::timezone-name zone)
                     (unix-to-timestamp time)))))))

(deftest test/timezone/local-mean-time-before-first-transition ()
  ;; Before standard time a place kept local mean time, which tzdata
  ;; records as subzone 0.
  (is (equal (%offset-in "America/New_York" 1700 6 15) '(-17762 nil "LMT")))
  (is (equal (%offset-in "Australia/Sydney" 1700 6 15) '(36292 nil "LMT")))
  (is (equal (%offset-in "Europe/London" 1800 6 15) '(-75 nil "LMT")))
  ;; London adopted GMT in 1847, which only the 64-bit block records.
  (is (equal (%offset-in "Europe/London" 1850 1 15) '(0 nil "GMT"))))

(deftest test/timezone/adjust-across-dst-by-days ()
  (let* ((old (parse-timestring "2014-03-09T01:00:00.000000-05:00"))
         (new (timestamp+ old 1 :day eastern-tz)))
    (is (= (* 23 60 60) (timestamp-difference new old)))))

(deftest test/timezone/adjust-across-dst-by-hours ()
  (let* ((old (parse-timestring "2014-03-09T01:00:00.000000-05:00"))
         (new (timestamp+ old 24 :hour eastern-tz)))
    (is (= (* 24 60 60) (timestamp-difference new old)))))

(deftest test/timezone/timestamp-minimize-part ()
  (is (timestamp=
       (timestamp-minimize-part
	(encode-timestamp 999999999 59 59 1 14 3 2010 :timezone eastern-tz)
	:month
	:timezone eastern-tz)
       (encode-timestamp 0 0 0 0 1 1 2010 :timezone eastern-tz)))
  (is (timestamp=
       (timestamp-minimize-part
	(encode-timestamp 0 0 0 2 14 3 2010 :timezone eastern-tz)
	:month
	:timezone eastern-tz)
       (encode-timestamp 0 0 0 0 1 1 2010 :timezone eastern-tz))))

(deftest test/timezone/timestamp-maximize-part ()
  (is (timestamp=
       (timestamp-maximize-part
	(encode-timestamp 999999999 59 59 1 7 11 2010 :timezone eastern-tz)
	:month
	:timezone eastern-tz)
       (encode-timestamp 999999999 59 59 23 31 12 2010 :timezone eastern-tz)))
  (is (timestamp=
       (timestamp-maximize-part
	(encode-timestamp 0 0 0 2 7 11 2010 :timezone eastern-tz)
	:month
	:timezone eastern-tz)
       (encode-timestamp 999999999 59 59 23 31 12 2010 :timezone eastern-tz))))

(deftest test/leaps/tai-to-utc ()
  (let ((*default-timezone* utc-leaps))
    (is (= 1435708799
           (local-time::%adjust-sec-for-leap-seconds 1435708824)
           (local-time::%adjust-sec-for-leap-seconds 1435708825)))
    (is (= 1435708800
           (local-time::%adjust-sec-for-leap-seconds 1435708826)))))

(deftest test/abbrev-subzone/not-found ()
  (is (null (local-time:all-timezones-matching-subzone "FOO"))))

(deftest test/abbrev-subzone/find-bst ()
  (is (member
       "Europe/London"
       (local-time:timezones-matching-subzone "BST" (unix-to-timestamp 1585906626))
       :key (lambda (x) (local-time:zone-name (car x)))
       :test #'string=))
  ;; Some historical timezone
  (is (member
       "America/Atka"
       (local-time:all-timezones-matching-subzone "BST")
       :key (lambda (x) (local-time:zone-name (car x)))
       :test #'string=)))

(deftest test/abbrev-subzone/find-gmt ()
  ;; As of 2020-04-03
  (is (equal
       "Etc/Greenwich"
       (local-time:zone-name
        (caar (local-time:timezones-matching-subzone "GMT" (unix-to-timestamp 1585906626)))))))

(deftest test/abbrev-subzone/transition-time ()
  (flet ((transition-time (abbrev timestamp zone-name)
           (third (find zone-name (timezones-matching-subzone abbrev timestamp)
                        :key (lambda (x) (zone-name (car x)))
                        :test #'string=))))
    ;; Inside the table: the transition is one of the table's own.
    (is (= (transition-time "BST" (unix-to-timestamp 1585906626) "Europe/London")
           (timestamp-to-unix (encode-timestamp 0 0 0 1 29 3 2020 :timezone +utc-zone+))))
    ;; Past the table: the transition the rule generated for that year, not
    ;; the table's last one in 2037.
    (is (= (transition-time "BST"
                            (encode-timestamp 0 0 0 12 1 7 2050 :timezone +utc-zone+)
                            "Europe/London")
           (timestamp-to-unix (encode-timestamp 0 0 0 1 27 3 2050 :timezone +utc-zone+))))
    ;; Before the first transition there is none.
    (is (null (transition-time "LMT"
                               (encode-timestamp 0 0 0 12 1 7 1800 :timezone +utc-zone+)
                               "Europe/London")))))
