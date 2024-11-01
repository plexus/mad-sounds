(ns mad-sounds.sessions.connections
  (:require
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]))

(jack/ports)

(jack/connect!
 #{["Overtone:out_1"
    #{"AG06/AG03 Analog Stereo:playback_FL"
      "Dante USB I/O Module Analog Stereo:playback_FL"
      "AG06/AG03 Digital Stereo (IEC958):playback_FL"
      "AG06/AG03 Pro:playback_AUX0"
      "Family 17h/19h HD Audio Controller Pro:playback_AUX0"
      "Family 17h/19h HD Audio Controller Speaker + Headphones:playback_FL"
      "oso:in"
      "jmeters:in-1"}]
   ["Overtone:out_2"
    #{"AG06/AG03 Analog Stereo:playback_FR"
      "Dante USB I/O Module Analog Stereo:playback_FR"
      "AG06/AG03 Digital Stereo (IEC958):playback_FR"
      "AG06/AG03 Pro:playback_AUX1"
      "Family 17h/19h HD Audio Controller Pro:playback_AUX1"
      "Family 17h/19h HD Audio Controller Speaker + Headphones:playback_FR"
      "oso:in"
      "jmeters:in-2"}]


   [#{"AG06/AG03 Analog Stereo:capture_FL"
      "Family 17h/19h HD Audio Controller Pro:capture_AUX0"
      #_"Family 17h/19h HD Audio Controller Digital Microphone:capture_FL"
      "Family 17h/19h HD Audio Controller Headphones Stereo Microphone:capture_FL"
      }
    "Overtone:in_1"]
   [#{"AG06/AG03 Analog Stereo:capture_FR"
      "Family 17h/19h HD Audio Controller Pro:capture_AUX1"
      #_"Family 17h/19h HD Audio Controller Digital Microphone:capture_FR"
      "Family 17h/19h HD Audio Controller Headphones Stereo Microphone:capture_FR"}
    "Overtone:in_2"]
   })
