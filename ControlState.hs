module ControlState where

data ControlState = ControlState { controlLeft :: Bool
                                 , controlRight :: Bool
                                 , controlFire :: Bool
                                 }

initialControlState = ControlState { controlLeft = False
                                   , controlRight = False
                                   , controlFire = False
                                   }
