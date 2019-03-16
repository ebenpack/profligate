module TestData where

import Data.List
import Data.Maybe
import ParseProf

costCenterStackCosts :: Forest CostCenterStackCosts
costCenterStackCosts =
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