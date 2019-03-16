module TestData where

import Prelude

import Data.Date as Date
import Data.DateTime as DateTime
import Data.Enum (toEnum)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time as Time
import Partial.Unsafe (unsafePartial)
import Prof (CostCenterStackCosts, PerCostCenterCosts, Forest, Tree(..))
import ParseProf (PerCostCenterCostsColumnWidths, CostCenterStackColumnWidths)

dateTime :: DateTime.DateTime
dateTime =
    let d1 = unsafePartial fromJust $ Date.canonicalDate <$> toEnum 2019 <*> pure Date.March <*> toEnum 11
        t1 = unsafePartial $ fromJust $ Time.Time <$> toEnum 7 <*> toEnum 33 <*> toEnum 0 <*> toEnum 0
        dt1 = DateTime.DateTime d1 t1
    in dt1

simplePerCostCenterCosts :: List PerCostCenterCosts
simplePerCostCenterCosts = fromFoldable 
    [ { name: "MAIN", mod: "MAIN", src: "<built-in>", time: 0.0, alloc: 68.8, ticks: Nothing, bytes: Nothing }
    , { name: "CAF" , mod: "GHC.IO.Handle.FD" , src: "<entire-module>" , time: 0.0 , alloc: 21.1, ticks: Nothing, bytes: Nothing }
    , { name: "CAF", mod: "GHC.IO.Encoding", src: "<entire-module>", time: 0.0, alloc: 1.7, ticks: Nothing, bytes: Nothing }
    , { name: "main", mod: "Main", src: "app/Main.hs:(77,1)-(80,27)", time: 0.0, alloc: 6.2, ticks: Nothing, bytes: Nothing }
    ]

detailedPerCostCenterCosts :: List PerCostCenterCosts
detailedPerCostCenterCosts = fromFoldable 
    [ { name: "MAIN", mod: "MAIN", src: "<built-in>", time: 100.0, alloc: 68.8, ticks: Just 2, bytes: Just 113160 }
    , { name: "CAF" , mod: "GHC.IO.Handle.FD" , src: "<entire-module>" , time: 0.0 , alloc: 21.1, ticks: Just 0, bytes: Just 34704 }
    , { name: "CAF", mod: "GHC.IO.Encoding", src: "<entire-module>", time: 0.0, alloc: 1.7, ticks: Just 0, bytes: Just 2768 }
    , { name: "main", mod: "Main", src: "app/Main.hs:(77,1)-(80,27)", time: 0.0, alloc: 6.2, ticks: Just 0, bytes: Just 10272 }
    ]

simplePerCostCenterCostsColumnWidth :: PerCostCenterCostsColumnWidths
simplePerCostCenterCostsColumnWidth = { name: 12, mod: 17, src: 28, time: 5, alloc: 7, ticks: 0, bytes: 0 }

simplePerCostCenterCostsLine :: PerCostCenterCosts
simplePerCostCenterCostsLine = { name: "main", mod: "Main", src: "app/Main.hs:(77,1)-(80,27)", time: 0.0, alloc: 6.2, ticks: Nothing, bytes: Nothing }

detailedPerCostCenterCostsColumnWidth :: PerCostCenterCostsColumnWidths
detailedPerCostCenterCostsColumnWidth = { name: 12, mod: 17, src: 28, time: 5, alloc: 7, ticks: 7, bytes: 10 }

detailedPerCostCenterCostsLine :: PerCostCenterCosts
detailedPerCostCenterCostsLine = { name: "main", mod: "Main", src: "app/Main.hs:(77,1)-(80,27)", time: 0.0, alloc: 6.2, ticks: Just 0, bytes: Just 10272 }

simpleCostCenterStackCostsColumnWidth :: CostCenterStackColumnWidths
simpleCostCenterStackCostsColumnWidth = { name: 13, mod: 22, src: 27, number: 9, entries: 7, individual: { time: 7, alloc: 7 }, inherited: { time: 8, alloc: 7 }, ticks: 0, bytes: 0 }

simpleCostCenterStackCostsLine :: CostCenterStackCosts
simpleCostCenterStackCostsLine = { name: "fib'.go", mod: "Main", src: "app/Main.hs:(72,5)-(74,51)", number: 2492, entries: 31, individual: { alloc: 0.6, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, ticks: Nothing, bytes: Nothing }

detailedCostCenterStackCostsColumnWidth :: CostCenterStackColumnWidths
detailedCostCenterStackCostsColumnWidth = { name: 13, mod: 22, src: 27, number: 9, entries: 7, individual: { time: 7, alloc: 7 }, inherited: { time: 8, alloc: 7 }, ticks: 7, bytes: 10 }

detailedCostCenterStackCostsLine :: CostCenterStackCosts
detailedCostCenterStackCostsLine = { name: "fib'.go", mod: "Main", src: "app/Main.hs:(72,5)-(74,51)", number: 2492, entries: 31, individual: { alloc: 0.6, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, ticks: Just 0, bytes: Just 928 }

simpleCostCenterStackCosts :: Forest CostCenterStackCosts
simpleCostCenterStackCosts =
    ( Node 
        { value: { bytes: Nothing, entries: 0, individual: { alloc: 68.8, time: 0.0 }, inherited: { alloc: 100.0, time: 0.0 }, mod: "MAIN", name: "MAIN", number: 1242, src: "<built-in>", ticks: Nothing }
        , children:
            ( Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.4, time: 0.0 }, inherited: { alloc: 0.4, time: 0.0 }, mod: "GHC.Conc.Signal", name: "CAF", number: 1341, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 1.7, time: 0.0 }, inherited: { alloc: 1.7, time: 0.0 }, mod: "GHC.IO.Encoding", name: "CAF", number: 1323, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.1, time: 0.0 }, inherited: { alloc: 0.1, time: 0.0 }, mod: "GHC.IO.Encoding.Iconv", name: "CAF", number: 1321, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 21.1, time: 0.0 }, inherited: { alloc: 21.1, time: 0.0 }, mod: "GHC.IO.Handle.FD", name: "CAF", number: 1312, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.1, time: 0.0 }, inherited: { alloc: 0.1, time: 0.0 }, mod: "GHC.IO.Handle.Text", name: "CAF", number: 1310, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "GHC.Event.Poll", name: "CAF", number: 1259, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.8, time: 0.0 }, inherited: { alloc: 0.8, time: 0.0 }, mod: "GHC.Event.Thread", name: "CAF", number: 1258, src: "<entire-module>", ticks: Nothing }
                , children: Nil
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "Main", name: "CAF:main", number: 2463, src: "app/Main.hs:77:1-4", ticks: Nothing }
                , children:
                    ( Node
                        { value: { bytes: Nothing, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "Main", name: "main", number: 2484, src: "app/Main.hs:(77,1)-(80,27)", ticks: Nothing }
                        , children: Nil
                        }
                    : Nil
                    )
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 1.0, time: 0.0 }, mod: "Main", name: "CAF:main1", number: 2462, src: "<no location info>", ticks: Nothing }
                , children:
                    ( Node
                        { value: { bytes: Nothing, entries: 0, individual: { alloc: 0.2, time: 0.0 }, inherited: { alloc: 1.0, time: 0.0 }, mod: "Main", name: "main", number: 2486, src: "app/Main.hs:(77,1)-(80,27)", ticks: Nothing }
                        , children:
                            ( Node
                                { value: { bytes: Nothing, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "main.f", number: 2487, src: "app/Main.hs:79:5-13", ticks: Nothing }
                                , children:
                                    ( Node
                                        { value: { bytes: Nothing, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "fib'", number: 2491, src: "app/Main.hs:(70,1)-(74,51)", ticks: Nothing }
                                        , children:
                                            ( Node
                                                { value: { bytes: Nothing, entries: 31, individual: { alloc: 0.6, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "fib'.go", number: 2492, src: "app/Main.hs:(72,5)-(74,51)", ticks: Nothing }
                                                , children: Nil
                                                }
                                            : Nil
                                            )
                                        }
                                    : Nil
                                    )
                                }
                            : Node
                                { value: { bytes: Nothing, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "main.g", number: 2488, src: "app/Main.hs:80:5-27", ticks: Nothing }
                                , children:
                                    ( Node
                                        { value: { bytes: Nothing, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "fib'", number: 2489, src: "app/Main.hs:(70,1)-(74,51)", ticks: Nothing }
                                        , children:
                                            ( Node
                                                { value: { bytes: Nothing, entries: 16, individual: { alloc: 0.3, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "fib'.go", number: 2490, src: "app/Main.hs:(72,5)-(74,51)", ticks: Nothing }
                                                , children: Nil
                                                }
                                            : Nil
                                            )
                                        }
                                    : Nil
                                    )
                                }
                            : Nil
                            )
                        }
                    : Nil
                    )
                }
            : Node
                { value: { bytes: Nothing, entries: 0, individual: { alloc: 6.0, time: 0.0 }, inherited: { alloc: 6.0, time: 0.0 }, mod: "Main", name: "main", number: 2485, src: "app/Main.hs:(77,1)-(80,27)", ticks: Nothing }
                , children: Nil
                }
            : Nil
            )
        }
    : Nil
    )

detailedCostCenterStackCosts :: Forest CostCenterStackCosts
detailedCostCenterStackCosts =
    ( Node 
        { value: { bytes: Just 113160, entries: 0, individual: { alloc: 68.8, time: 100.0 }, inherited: { alloc: 100.0, time: 100.0 }, mod: "MAIN", name: "MAIN", number: 1242, src: "<built-in>", ticks: Just 2 }
        , children:
            ( Node
                { value: { bytes: Just 640, entries: 0, individual: { alloc: 0.4, time: 0.0 }, inherited: { alloc: 0.4, time: 0.0 }, mod: "GHC.Conc.Signal", name: "CAF", number: 1341, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 2768, entries: 0, individual: { alloc: 1.7, time: 0.0 }, inherited: { alloc: 1.7, time: 0.0 }, mod: "GHC.IO.Encoding", name: "CAF", number: 1323, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 200, entries: 0, individual: { alloc: 0.1, time: 0.0 }, inherited: { alloc: 0.1, time: 0.0 }, mod: "GHC.IO.Encoding.Iconv", name: "CAF", number: 1321, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 34704, entries: 0, individual: { alloc: 21.1, time: 0.0 }, inherited: { alloc: 21.1, time: 0.0 }, mod: "GHC.IO.Handle.FD", name: "CAF", number: 1312, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 88, entries: 0, individual: { alloc: 0.1, time: 0.0 }, inherited: { alloc: 0.1, time: 0.0 }, mod: "GHC.IO.Handle.Text", name: "CAF", number: 1310, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 48, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "GHC.Event.Poll", name: "CAF", number: 1259, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 1272, entries: 0, individual: { alloc: 0.8, time: 0.0 }, inherited: { alloc: 0.8, time: 0.0 }, mod: "GHC.Event.Thread", name: "CAF", number: 1258, src: "<entire-module>", ticks: Just 0 }
                , children: Nil
                }
            : Node
                { value: { bytes: Just 0, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "Main", name: "CAF:main", number: 2463, src: "app/Main.hs:77:1-4", ticks: Just 0 }
                , children:
                    ( Node
                        { value: { bytes: Just 48, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.0, time: 0.0 }, mod: "Main", name: "main", number: 2484, src: "app/Main.hs:(77,1)-(80,27)", ticks: Just 0 }
                        , children: Nil
                        }
                    : Nil
                    )
                }
            : Node
                { value: { bytes: Just 0, entries: 0, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 1.0, time: 0.0 }, mod: "Main", name: "CAF:main1", number: 2462, src: "<no location info>", ticks: Just 0 }
                , children:
                    ( Node
                        { value: { bytes: Just 296, entries: 0, individual: { alloc: 0.2, time: 0.0 }, inherited: { alloc: 1.0, time: 0.0 }, mod: "Main", name: "main", number: 2486, src: "app/Main.hs:(77,1)-(80,27)", ticks: Just 0 }
                        , children:
                            ( Node
                                { value: { bytes: Just 0, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "main.f", number: 2487, src: "app/Main.hs:79:5-13", ticks: Just 0 }
                                , children:
                                    ( Node
                                        { value: { bytes: Just 0, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "fib'", number: 2491, src: "app/Main.hs:(70,1)-(74,51)", ticks: Just 0 }
                                        , children:
                                            ( Node
                                                { value: { bytes: Just 928, entries: 31, individual: { alloc: 0.6, time: 0.0 }, inherited: { alloc: 0.6, time: 0.0 }, mod: "Main", name: "fib'.go", number: 2492, src: "app/Main.hs:(72,5)-(74,51)", ticks: Just 0 }
                                                , children: Nil
                                                }
                                            : Nil
                                            )
                                        }
                                    : Nil
                                    )
                                }
                            : Node
                                { value: { bytes: Just 0, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "main.g", number: 2488, src: "app/Main.hs:80:5-27", ticks: Just 0 }
                                , children:
                                    ( Node
                                        { value: { bytes: Just 0, entries: 1, individual: { alloc: 0.0, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "fib'", number: 2489, src: "app/Main.hs:(70,1)-(74,51)", ticks: Just 0 }
                                        , children:
                                            ( Node
                                                { value: { bytes: Just 448, entries: 16, individual: { alloc: 0.3, time: 0.0 }, inherited: { alloc: 0.3, time: 0.0 }, mod: "Main", name: "fib'.go", number: 2490, src: "app/Main.hs:(72,5)-(74,51)", ticks: Just 0 }
                                                , children: Nil
                                                }
                                            : Nil
                                            )
                                        }
                                    : Nil
                                    )
                                }
                            : Nil
                            )
                        }
                    : Nil
                    )
                }
            : Node
                { value: { bytes: Just 9928, entries: 0, individual: { alloc: 6.0, time: 0.0 }, inherited: { alloc: 6.0, time: 0.0 }, mod: "Main", name: "main", number: 2485, src: "app/Main.hs:(77,1)-(80,27)", ticks: Just 0 }
                , children: Nil
                }
            : Nil
            )
        }
    : Nil
    )