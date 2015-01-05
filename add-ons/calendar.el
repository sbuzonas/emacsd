(defaddon calendar
  nil


  ;; Set location information for sunrise capability
  (setq calendar-latitude 40.4)
  (setq calendar-longitude -80.0)
  (setq calendar-location-name "Pittsburgh, PA")

  ;; Fix time zone
  (setq calendar-time-zone -300)
  (setq calendar-standard-time-zone-name "EST")
  (setq calendar-daylight-time-zone-name "EDT"))
