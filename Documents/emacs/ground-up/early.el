;; Functions and variables required by packages.el

;; Functions
(defun hour-minute-between (hour-start minute-start hour-end minute-end hour minute)
  "Checks whether (HOUR, MINUTE) are between (inclusive) the ranges 
(HOUR-START, MINUTE-START) and (HOUR-END, MINUTE-END)"
  (and (>= hour hour-start) (<= hour hour-end)
       (or (/= hour hour-start) (>= minute minute-start))
       (or (/= hour hour-end) (<= minute minute-end))))

(defun greet-other-lang ()
  "Greet based on the time of the day"
  (let ((hour (string-to-number (format-time-string "%H")))
        (minute (string-to-number (format-time-string "%M"))))
    (if (hour-minute-between 6 0 11 59 hour minute)
        (concat "𑀰𑀼 𑀪 𑀧𑁆𑀭𑀪𑀸𑀢 " user-full-name)
      (if (and (= hour 12) (= minute 0))
          (concat "𑀅𑀩 𑀤𑁄𑀧𑀳𑀭 𑀳𑁄 𑀕𑀈 𑀳𑁃, " user-full-name)
        (if (hour-minute-between 12 1 17 0 hour minute)
            (concat "𑀦𑀫𑀲𑁆𑀓𑀸𑀭 " user-full-name)
          (if (hour-minute-between 17 1 20 0 hour minute)
              (concat "𑀲𑀼𑀲𑀁𑀥𑁆𑀬𑀸 " user-full-name)
            (concat "𑀰𑀼 𑀪 𑀭𑀸𑀢𑁆𑀭𑀺 " user-full-name)))))))

;; Variables
