module Mescaline.Pattern.Patch.Default where

import Data.IntMap (fromList)
import Mescaline.Pattern.AST

defaultTree :: (String, Tree Event)
defaultTree = (src, tree)
    where
        src = "it = patch $ par $ flip List.map [0..7] $ \\i ->\n"
           ++ "  let tick = 0.125\n"
           ++ "  in sequencer tick (region i tick (rand 0 1))"
        tree = Tree (fromList []) (fromList []) (E_par [E_bind_E 0 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 0 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 0)) (S_value 0.0)) (E_binding 0)),E_bind_E 1 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 1 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 1)) (S_value 0.0)) (E_binding 1)),E_bind_E 2 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 2 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 2)) (S_value 0.0)) (E_binding 2)),E_bind_E 3 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 3 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 3)) (S_value 0.0)) (E_binding 3)),E_bind_E 4 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 4 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 4)) (S_value 0.0)) (E_binding 4)),E_bind_E 5 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 5 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 5)) (S_value 0.0)) (E_binding 5)),E_bind_E 6 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 6 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 6)) (S_value 0.0)) (E_binding 6)),E_bind_E 7 (E_step (S_value 0.0) (S_value 1.0) (E_set Delta (S_value 0.125) (E_region 7 (S_value 0.125) (S_rand (S_value 0.0) (S_value 1.0))))) (E_filter (B_compare Comp_gt (S_get CursorValue (E_binding 7)) (S_value 0.0)) (E_binding 7))])
