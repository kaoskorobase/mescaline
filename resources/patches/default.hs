it = patch $ par $ flip List.map [0..7] $ \i ->
        let tick = 0.125
        in sequencer tick $
            -- bind (rand 0 (radius (fromIntegral i))) $
            --     \r -> closest i
            --             tick
            --             (polar (center (fromIntegral i)) r (rand 0 (2*pi)))
            --             -- (radius (constant i) - r)
            --             (1/0)
            region Uniform i tick (fromIntegral i)