it = patch $ par $ flip List.map [0..7] $ \i ->
        let tick = 0.125
        in sequencer tick (region i tick (rand 0 1))
