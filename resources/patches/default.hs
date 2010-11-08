it = patch $ par $ P.map (\i -> region Uniform (constant i) (sequencer i (constant tick))) [0..7]
    where tick = 0.125
