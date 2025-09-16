-- |

module Modelling.CdOd.SelectValidCd.Instance where

import qualified Data.Map                         as M (fromList)

import Modelling.CdOd.SelectValidCd (
  SelectValidCdInstance (..),
  SelectValidCdTaskTextElement (..),
  )

import Modelling.Auxiliary.Shuffle.All  (ShuffleInstance (..))
import Modelling.CdOd.RepairCd          (InValidOption (..))
import Modelling.CdOd.Types (
  Annotation (..),
  AnyClassDiagram (..),
  CdDrawSettings (..),
  LimitedLinking (..),
  Link (..),
  Object (..),
  ObjectDiagram (..),
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  )
import Modelling.Types                  (Change (..))

import Control.OutputCapable.Blocks (
  ArticleToUse (DefiniteArticle),
  Language (English, German),
  )
import Control.OutputCapable.Blocks.Generic.Type (
  GenericOutput (Code, Paragraph, Special, Translated),
  )
import Data.Map                         (Map)

listToFM :: Ord a => [(a, b)] -> Map a b
listToFM = M.fromList

{-|
demo task
-}
task2024_05 :: SelectValidCdInstance
task2024_05 = SelectValidCdInstance {
  cdDrawSettings = CdDrawSettings {
    omittedDefaults = OmittedDefaultMultiplicities {
      aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
      associationOmittedDefaultMultiplicity = Just (0, Nothing),
      compositionWholeOmittedDefaultMultiplicity = Just (1, Just 1)
      },
    printNames = True,
    printNavigations = True
    },
  classDiagrams = listToFM [
    (1, InValidOption {
      hint = Left (Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just (Right (Inheritance {
            subClass = "D",
            superClass = "C"
            }))
          },
        annotation = DefiniteArticle
        }),
      option = AnyClassDiagram {
        anyClassNames = ["B", "C", "D", "A"],
        anyRelationships = [
          Right (Inheritance {subClass = "D", superClass = "C"}),
          Right (Inheritance {subClass = "C", superClass = "D"}),
          Right (Inheritance {subClass = "B", superClass = "D"}),
          Right (Inheritance {subClass = "A", superClass = "B"})
          ]
        }
      }),
    (2, InValidOption {
      hint = Right (ObjectDiagram {
        objects = [
          Object {
            isAnonymous = False,
            objectName = "b",
            objectClass = "B"
            },
          Object {
            isAnonymous = False,
            objectName = "c",
            objectClass = "C"
            },
          Object {
            isAnonymous = False,
            objectName = "d",
            objectClass = "D"
            },
          Object {
            isAnonymous = False,
            objectName = "a",
            objectClass = "A"
            }
          ],
        links = []
        }),
      option = AnyClassDiagram {
          anyClassNames = ["B", "A", "D", "C"],
        anyRelationships = [
          Right (Inheritance {subClass = "C", superClass = "D"}),
          Right (Inheritance {subClass = "A", superClass = "B"})
          ]
        }
      }),
    (3, InValidOption {
      hint = Left (Annotation {
        annotated = Change {
          add = Nothing,
          remove = Just (Right (Inheritance {
            subClass = "D",
            superClass = "C"
            }))
          },
        annotation = DefiniteArticle
        }),
      option = AnyClassDiagram {
        anyClassNames = ["B", "D", "C", "A"],
        anyRelationships = [
          Right (Inheritance {subClass = "B", superClass = "A"}),
          Right (Inheritance {subClass = "C", superClass = "D"}),
          Right (Inheritance {subClass = "D", superClass = "C"})
          ]
        }
    }),
    (4, InValidOption {
      hint = Right (ObjectDiagram {
        objects = [
          Object {
            isAnonymous = False,
            objectName = "a",
            objectClass = "A"
            },
          Object {
            isAnonymous = False,
            objectName = "d",
            objectClass = "D"
            },
          Object {
            isAnonymous = False,
            objectName = "b",
            objectClass = "B"
            },
          Object {
            isAnonymous = False,
            objectName = "c",
            objectClass = "C"
            }
          ],
        links = []
        }),
      option = AnyClassDiagram {
        anyClassNames = ["D", "C", "B", "A"],
        anyRelationships = [
          Right (Inheritance {subClass = "A", superClass = "B"}),
          Right (Inheritance {subClass = "D", superClass = "C"})
          ]
        }
      })
    ],
  showExtendedFeedback = True,
  showSolution = True,
  taskText = [
    Paragraph [
      Translated (listToFM [
        (English, "Consider the following class diagram candidates:"),
        (German, "Betrachten Sie die folgenden Klassendiagrammkandidaten:")
        ])
      ],
    Special CdCandidates,
    Paragraph [
      Translated (listToFM [
        (English, "Which of these class diagram candidates are valid class diagrams?\nPlease state your answer by giving a list of numbers, indicating all valid class diagrams."),
        (German, "Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?\nBitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.")
        ])
      ],
    Paragraph [
      Translated (listToFM [
        (English, "For example,"),
        (German, "Zum Beispiel würde")
        ]),
      Code (listToFM [
        (English, "[1, 2]"),
        (German, "[1, 2]")
        ]),
      Translated (listToFM [
        (English, "would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams."),
        (German, "bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.")
        ])
      ]
    ],
  addText = Nothing
  }

{-|
Picked because generation for @task2024_06@ took too long to generate.
-}
task2024_06picked :: ShuffleInstance SelectValidCdInstance
task2024_06picked = ShuffleInstance {
  taskInstance = SelectValidCdInstance {
    cdDrawSettings = CdDrawSettings {
      omittedDefaults = OmittedDefaultMultiplicities {
        aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
        associationOmittedDefaultMultiplicity = Just (0, Nothing),
        compositionWholeOmittedDefaultMultiplicity = Nothing
        },
      printNames = True,
      printNavigations = False
      },
    classDiagrams = listToFM [
      (1, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object { isAnonymous = False, objectName = "c", objectClass = "C" },
            Object { isAnonymous = False, objectName = "a", objectClass = "A" },
            Object { isAnonymous = True, objectName = "e", objectClass = "E" },
            Object { isAnonymous = False, objectName = "b", objectClass = "B" },
            Object { isAnonymous = False, objectName = "d", objectClass = "D" }
            ],
          links = [
            Link { linkLabel = "s", linkFrom = "d", linkTo = "b" },
            Link { linkLabel = "z", linkFrom = "a", linkTo = "d" },
            Link { linkLabel = "v", linkFrom = "a", linkTo = "c" },
            Link { linkLabel = "y", linkFrom = "d", linkTo = "c" },
            Link { linkLabel = "u", linkFrom = "a", linkTo = "b" }
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["B", "C", "D", "A", "E"],
          anyRelationships = [
            Right (Aggregation {
              aggregationName = "z",
              aggregationPart = LimitedLinking { linking = "A", limits = (0, Just 1) },
              aggregationWhole = LimitedLinking { linking = "D", limits = (1, Nothing) }
              }),
            Right (Composition {
              compositionName = "v",
              compositionPart = LimitedLinking { linking = "A", limits = (0, Just 2) },
              compositionWhole = LimitedLinking { linking = "C", limits = (0, Just 1) }
              }),
            Right (Association {
              associationName = "u",
              associationFrom = LimitedLinking { linking = "A", limits = (1, Just 1) },
              associationTo = LimitedLinking { linking = "B", limits = (0, Nothing) }
              }),
            Right (Composition {
              compositionName = "y",
              compositionPart = LimitedLinking { linking = "D", limits = (1, Just 1) },
              compositionWhole = LimitedLinking { linking = "C", limits = (0, Just 1) }
              }),
            Right (Inheritance { subClass = "C", superClass = "E" }),
            Right (Inheritance { subClass = "D", superClass = "E" }),
            Right (Association {
              associationName = "s",
              associationFrom = LimitedLinking { linking = "E", limits = (0, Nothing) },
              associationTo = LimitedLinking { linking = "B", limits = (0, Nothing) }
              })
            ]
          }
        }),
      (2, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Inheritance { subClass = "A", superClass = "B" }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["B", "A", "C", "D", "E"],
          anyRelationships = [
            Right (Aggregation {
              aggregationName = "y",
              aggregationPart = LimitedLinking { linking = "E", limits = (0, Just 1) },
              aggregationWhole = LimitedLinking { linking = "A", limits = (1, Nothing) }
              }),
            Right (Composition {
              compositionName = "s",
              compositionPart = LimitedLinking { linking = "A", limits = (1, Just 1) },
              compositionWhole = LimitedLinking { linking = "D", limits = (0, Just 1) }
              }),
            Right (Composition {
              compositionName = "w",
              compositionPart = LimitedLinking { linking = "D", limits = (0, Just 2) },
              compositionWhole = LimitedLinking { linking = "E", limits = (0, Just 1) }
              }),
            Right (Association {
              associationName = "u",
              associationFrom = LimitedLinking { linking = "E", limits = (1, Just 1) },
              associationTo = LimitedLinking { linking = "C", limits = (0, Nothing) }
              }),
            Right (Association {
              associationName = "t",
              associationFrom = LimitedLinking { linking = "B", limits = (0, Nothing) },
              associationTo = LimitedLinking { linking = "C", limits = (0, Nothing) }
              }),
            Right (Inheritance { subClass = "A", superClass = "B" }),
            Right (Inheritance { subClass = "B", superClass = "D" })
            ]
          }
        }),
      (3, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Inheritance { subClass = "D", superClass = "E" }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["A", "D", "C", "E", "B"],
          anyRelationships = [
            Right (Composition {
              compositionName = "t",
              compositionPart = LimitedLinking { linking = "B", limits = (0, Just 2) },
              compositionWhole = LimitedLinking { linking = "E", limits = (0, Just 1) }
              }),
            Right (Aggregation {
              aggregationName = "u",
              aggregationPart = LimitedLinking { linking = "A", limits = (1, Just 2) },
              aggregationWhole = LimitedLinking { linking = "B", limits = (1, Just 2) }
              }),
            Right (Inheritance { subClass = "C", superClass = "D" }),
            Right (Association {
              associationName = "v",
              associationFrom = LimitedLinking { linking = "B", limits = (1, Just 1) },
              associationTo = LimitedLinking { linking = "A", limits = (0, Nothing) }
              }),
            Right (Composition {
              compositionName = "y",
              compositionPart = LimitedLinking { linking = "C", limits = (1, Just 1) },
              compositionWhole = LimitedLinking { linking = "E", limits = (0, Just 1) }
              }),
            Right (Aggregation {
              aggregationName = "x",
              aggregationPart = LimitedLinking { linking = "B", limits = (0, Just 1) },
              aggregationWhole = LimitedLinking { linking = "C", limits = (1, Nothing) }
              }),
            Right (Association {
              associationName = "w",
              associationFrom = LimitedLinking { linking = "D", limits = (0, Nothing) },
              associationTo = LimitedLinking { linking = "A", limits = (0, Nothing) }
              }),
            Right (Inheritance { subClass = "D", superClass = "E" })
            ]
          }
        }),
      (4, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object { isAnonymous = True, objectName = "e", objectClass = "E" },
            Object { isAnonymous = False, objectName = "d", objectClass = "D" },
            Object { isAnonymous = False, objectName = "a", objectClass = "A" },
            Object { isAnonymous = False, objectName = "b", objectClass = "B" },
            Object { isAnonymous = False, objectName = "c", objectClass = "C" }
            ],
          links = [
            Link { linkLabel = "w", linkFrom = "b", linkTo = "d" },
            Link { linkLabel = "x", linkFrom = "b", linkTo = "e" },
            Link { linkLabel = "v", linkFrom = "c", linkTo = "d" },
            Link { linkLabel = "t", linkFrom = "e", linkTo = "a" },
            Link { linkLabel = "s", linkFrom = "b", linkTo = "a" },
            Link { linkLabel = "z", linkFrom = "e", linkTo = "d" }
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["D", "E", "A", "B", "C"],
          anyRelationships = [
            Right (Association {
              associationName = "s",
              associationFrom = LimitedLinking { linking = "B", limits = (1, Just 1) },
              associationTo = LimitedLinking { linking = "A", limits = (0, Nothing) }
              }),
            Right (Composition {
              compositionName = "w",
              compositionPart = LimitedLinking { linking = "B", limits = (0, Just 2) },
              compositionWhole = LimitedLinking { linking = "D", limits = (0, Just 1) }
              }),
            Right (Aggregation {
              aggregationName = "x",
              aggregationPart = LimitedLinking { linking = "B", limits = (0, Just 1) },
              aggregationWhole = LimitedLinking { linking = "E", limits = (1, Nothing) }
              }),
            Right (Composition {
              compositionName = "z",
              compositionPart = LimitedLinking { linking = "E", limits = (1, Just 1) },
              compositionWhole = LimitedLinking { linking = "D", limits = (0, Just 1) }
              }),
            Right (Inheritance { subClass = "E", superClass = "C" }),
            Right (Association {
              associationName = "t",
              associationFrom = LimitedLinking { linking = "C", limits = (0, Nothing) },
              associationTo = LimitedLinking { linking = "A", limits = (0, Nothing) }
              }),
            Right (Aggregation {
              aggregationName = "v",
              aggregationPart = LimitedLinking { linking = "C", limits = (0, Nothing) },
              aggregationWhole = LimitedLinking { linking = "D", limits = (0, Just 2) }
              })
            ]
          }
        })
      ],
    showExtendedFeedback = True,
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "Consider the following class diagram candidates:"),
          (German, "Betrachten Sie die folgenden Klassendiagrammkandidaten:")
          ])
        ],
      Special CdCandidates,
      Paragraph [
        Translated (listToFM [
          (English, "Which of these class diagram candidates are valid class diagrams?\nPlease state your answer by giving a list of numbers, indicating all valid class diagrams."),
          (German, "Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?\nBitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "For example,"),
          (German, "Zum Beispiel würde")
          ]),
        Code (listToFM [
          (English, "[1, 2]"),
          (German, "[1, 2]")
          ]),
        Translated (listToFM [
          (English, "would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams."),
          (German, "bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.")
          ])
        ]
      ],
    addText = Nothing
    },
  allowLayoutMangling = True,
  shuffleNames = True,
  shuffleOptions = True
  }

{-|
points: 0.15
-}
task2024_09 :: ShuffleInstance SelectValidCdInstance
task2024_09 = ShuffleInstance {
  taskInstance = SelectValidCdInstance {
    cdDrawSettings = CdDrawSettings {
      omittedDefaults = OmittedDefaultMultiplicities {
        aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
        associationOmittedDefaultMultiplicity = Just (0, Nothing),
        compositionWholeOmittedDefaultMultiplicity = Nothing
        },
      printNames = False,
      printNavigations = True
      },
    classDiagrams = listToFM [
      (1, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object {
              isAnonymous = False,
              objectName = "a",
              objectClass = "A"
              },
            Object {
              isAnonymous = False,
              objectName = "b",
              objectClass = "B"
              },
            Object {
              isAnonymous = False,
              objectName = "d",
              objectClass = "D"
              },
            Object {
              isAnonymous = False,
              objectName = "e",
              objectClass = "E"
              },
            Object {
              isAnonymous = False,
              objectName = "c",
              objectClass = "C"
              }
            ],
          links = [
            Link { linkLabel = "u", linkFrom = "a", linkTo = "d" },
            Link { linkLabel = "r", linkFrom = "e", linkTo = "b" },
            Link { linkLabel = "t", linkFrom = "a", linkTo = "e" },
            Link { linkLabel = "y", linkFrom = "e", linkTo = "c" },
            Link { linkLabel = "z", linkFrom = "b", linkTo = "e" },
            Link { linkLabel = "r", linkFrom = "d", linkTo = "b" },
            Link { linkLabel = "q", linkFrom = "e", linkTo = "c" }
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["C", "B", "E", "D", "A"],
          anyRelationships = [
            Right (Association {
              associationName = "y",
              associationFrom = (LimitedLinking {
                linking = "E", limits = (0, Just 1)
                }),
              associationTo = (LimitedLinking {
                linking = "C", limits = (0, Just 2)
                })
              }),
            Right (Composition {
              compositionName = "r",
              compositionPart = (LimitedLinking {
                linking = "D", limits = (2, Just 2)
                }),
              compositionWhole = (LimitedLinking {
                linking = "B", limits = (0, Just 1)
                })
              }),
            Right (Association {
              associationName = "q",
              associationFrom = (LimitedLinking {
                linking = "D", limits = (0, Just 1)
                }),
              associationTo = (LimitedLinking {
                linking = "C", limits = (0, Nothing)
                })
              }),
            Right (Inheritance { subClass = "E", superClass = "D" }),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart = (LimitedLinking {
                linking = "A", limits = (0, Just 1)
                }),
              aggregationWhole = (LimitedLinking {
                linking = "E", limits = (0, Nothing)
                })
              }),
            Right (Composition {
              compositionName = "u",
              compositionPart = (LimitedLinking {
                linking = "A", limits = (0, Nothing)
                }),
              compositionWhole = (LimitedLinking {
                linking = "D", limits = (0, Just 1)
                })
              }),
            Right (Aggregation {
              aggregationName = "z",
              aggregationPart = (LimitedLinking {
                linking = "B", limits = (0, Just 1)
                }),
              aggregationWhole = (LimitedLinking {
                linking = "E", limits = (0, Just 1)
                })
              })
            ]
          }
        }),
      (2, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Composition {
              compositionName = "z",
              compositionPart = (LimitedLinking {
                linking = "E", limits = (0, Just 2)
                }),
              compositionWhole = (LimitedLinking {
                linking = "A", limits = (1, Just 1)
                })
              }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["B", "D", "E", "C", "A"],
          anyRelationships = [
            Right (Aggregation {
              aggregationName = "s",
              aggregationPart = (LimitedLinking {
                linking = "E", limits = (0, Just 1)
                }),
              aggregationWhole = (LimitedLinking {
                linking = "D", limits = (0, Just 1)
                })
              }),
            Right (Association {
              associationName = "v",
              associationFrom = (LimitedLinking {
                linking = "A", limits = (0, Just 1)
                }),
              associationTo = (LimitedLinking {
                linking = "B", limits = (0, Nothing)
                })
              }),
            Right (Inheritance { subClass = "D", superClass = "A" }),
            Right (Composition {
              compositionName = "r",
              compositionPart = (LimitedLinking {
                linking = "C", limits = (0, Nothing)
                }),
              compositionWhole = (LimitedLinking {
                linking = "A", limits = (0, Just 1)
                })
              }),
            Right (Aggregation {
              aggregationName = "w",
              aggregationPart = (LimitedLinking {
                linking = "C", limits = (0, Just 1)
                }),
              aggregationWhole = (LimitedLinking {
                linking = "D", limits = (0, Nothing)
                })
              }),
            Right (Association {
              associationName = "u",
              associationFrom = (LimitedLinking {
                linking = "D", limits = (0, Just 1)
                }),
              associationTo = (LimitedLinking {
                linking = "B", limits = (0, Just 2)
                })
              }),
            Right (Composition {
              compositionName = "q",
              compositionPart = (LimitedLinking {
                linking = "E", limits = (2, Just 2)
                }),
              compositionWhole = (LimitedLinking {
                linking = "B", limits = (1, Just 1)
                })
              }),
            Right (Composition {
              compositionName = "z",
              compositionPart = (LimitedLinking {
                linking = "E", limits = (0, Just 2)
                }),
              compositionWhole = (LimitedLinking {
                linking = "A", limits = (1, Just 1)
                })
              })
            ]
          }
        }),
      (3, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Composition {
              compositionName = "z",
              compositionPart = LimitedLinking {
                linking = "A", limits = (2, Just 2)
                },
              compositionWhole = LimitedLinking {
                linking = "C", limits = (1, Just 2)
                }
              }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["B", "A", "E", "C", "D"],
          anyRelationships = [
            Right (Composition {
              compositionName = "w",
              compositionPart = LimitedLinking {
                linking = "E", limits = (2, Just 2)
                },
              compositionWhole = LimitedLinking {
                linking = "A", limits = (0, Just 1)
                }
              }),
            Right (Association {
              associationName = "u",
              associationFrom = LimitedLinking {
                linking = "A", limits = (0, Just 1)
                },
              associationTo = LimitedLinking {
                linking = "E", limits = (0, Nothing)
                }
              }),
            Right (Association {
              associationName = "x",
              associationFrom = LimitedLinking {
                linking = "B", limits = (0, Just 1)
                },
              associationTo = LimitedLinking {
                linking = "E", limits = (0, Just 2)
                }
              }),
            Right (Composition {
              compositionName = "z",
              compositionPart = LimitedLinking {
                linking = "A", limits = (2, Just 2)
                },
              compositionWhole = LimitedLinking {
                linking = "C", limits = (1, Just 2)
                }
              }),
            Right (Composition {
              compositionName = "s",
              compositionPart = LimitedLinking {
                linking = "D", limits = (0, Nothing)
                },
              compositionWhole = LimitedLinking {
                linking = "A", limits = (0, Just 1)
                }
              }),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart = LimitedLinking {
                linking = "C", limits = (0, Just 1)
                },
              aggregationWhole = LimitedLinking {
                linking = "B", limits = (0, Just 1)
                }
              }),
            Right (Inheritance {
              subClass = "B",
              superClass = "A"
              }),
            Right (Aggregation {
              aggregationName = "v",
              aggregationPart = LimitedLinking {
                linking = "D", limits = (0, Just 1)
                },
              aggregationWhole = LimitedLinking {
                linking = "B", limits = (0, Nothing)
                }
              })
            ]
          }
        }),
      (4, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object {
              isAnonymous = False,
              objectName = "c",
              objectClass = "C"
              },
            Object {
              isAnonymous = False,
              objectName = "e",
              objectClass = "E"
              },
            Object {
              isAnonymous = False,
              objectName = "a",
              objectClass = "A"
              },
            Object {
              isAnonymous = False,
              objectName = "b",
              objectClass = "B"
              },
            Object {
              isAnonymous = False,
              objectName = "d",
              objectClass = "D"
              }
            ],
          links = [
            Link { linkLabel = "s", linkFrom = "c", linkTo = "d" },
            Link { linkLabel = "t", linkFrom = "c", linkTo = "e" },
            Link { linkLabel = "r", linkFrom = "a", linkTo = "b" },
            Link { linkLabel = "t", linkFrom = "b", linkTo = "e" },
            Link { linkLabel = "q", linkFrom = "a", linkTo = "c" },
            Link { linkLabel = "y", linkFrom = "c", linkTo = "d" },
            Link { linkLabel = "z", linkFrom = "e", linkTo = "c" }
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["D", "C", "B", "E", "A"],
          anyRelationships = [
            Right (Aggregation {
              aggregationName = "q",
              aggregationPart = LimitedLinking {
                linking = "A", limits = (0, Just 1)
                },
              aggregationWhole = LimitedLinking {
                linking = "C", limits = (0, Nothing)
                }
              }),
            Right (Association {
              associationName = "s",
              associationFrom = LimitedLinking {
                linking = "C", limits = (0, Just 1)
                },
              associationTo = LimitedLinking {
                linking = "D", limits = (0, Just 2)
                }
              }),
            Right (Association {
              associationName = "y",
              associationFrom = LimitedLinking {
                linking = "B", limits = (0, Just 1)
                },
              associationTo = LimitedLinking {
                linking = "D", limits = (0, Nothing)
                }
              }),
            Right (Composition {
              compositionName = "r",
              compositionPart = LimitedLinking {
                linking = "A", limits = (0, Nothing)
                },
              compositionWhole = LimitedLinking {
                linking = "B", limits = (0, Just 1)
                }
              }),
            Right (Inheritance {
              subClass = "C",
              superClass = "B"
              }),
            Right (Composition {
              compositionName = "t",
              compositionPart = LimitedLinking {
                linking = "B", limits = (2, Just 2)
                },
              compositionWhole = LimitedLinking {
                linking = "E", limits = (1, Just 1)
                }
              }),
            Right (Aggregation {
              aggregationName = "z",
              aggregationPart = LimitedLinking {
                linking = "E", limits = (0, Just 1)
                },
              aggregationWhole = LimitedLinking {
                linking = "C", limits = (0, Just 1)
                }
              })
            ]
          }
        })
      ],
    showExtendedFeedback = True,
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "Consider the following class diagram candidates:"),
          (German, "Betrachten Sie die folgenden Klassendiagrammkandidaten:")
          ])
        ],
      Special CdCandidates,
      Paragraph [
        Translated (listToFM [
          (English, "Which of these class diagram candidates are valid class diagrams?\nPlease state your answer by giving a list of numbers, indicating all valid class diagrams."),
          (German, "Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?\nBitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "For example,"), (German, "Zum Beispiel würde")
          ]),
        Code (listToFM [
          (English, "[1, 2]"), (German, "[1, 2]")
          ]),
        Translated (listToFM [
          (English, "would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams."),
          (German, "bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.")
          ])
        ]
      ],
    addText = Nothing
    },
  allowLayoutMangling = True,
  shuffleNames = True,
  shuffleOptions = True
  }

{-|
points: 0.08
-}
task2024_53 :: ShuffleInstance SelectValidCdInstance
task2024_53 = ShuffleInstance {
  taskInstance = SelectValidCdInstance {
    cdDrawSettings = CdDrawSettings {
      omittedDefaults = OmittedDefaultMultiplicities {
        aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
        associationOmittedDefaultMultiplicity = Just (0, Nothing),
        compositionWholeOmittedDefaultMultiplicity = Nothing
        },
      printNames = False,
      printNavigations = False
      },
    classDiagrams = listToFM [
      (1, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Composition {
              compositionName = "u",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (1, Just 1)}
              }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["E", "C", "B", "A", "D"],
          anyRelationships = [
            Right (Inheritance {subClass = "E", superClass = "C"}),
            Right (Composition {
              compositionName = "y",
              compositionPart =
                LimitedLinking {linking = "C", limits = (0, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "D", limits = (1, Just 1)}
              }),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart =
                LimitedLinking {linking = "B", limits = (0, Just 2)},
              aggregationWhole =
                LimitedLinking {linking = "A", limits = (1, Just 2)}
              }),
            Right (Composition {
              compositionName = "u",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (1, Just 1)}
              }),
            Right (Association {
              associationName = "v",
              associationFrom =
                LimitedLinking {linking = "D", limits = (1, Just 1)},
              associationTo =
                LimitedLinking {linking = "B", limits = (1, Nothing)}
              }),
            Right (Aggregation {
              aggregationName = "w",
              aggregationPart =
                LimitedLinking {linking = "A", limits = (0, Just 1)},
              aggregationWhole =
                LimitedLinking {linking = "C", limits = (1, Just 1)}
              }),
            Right (Inheritance {subClass = "D", superClass = "A"})
            ]
          }
        }),
      (2, InValidOption {
        hint = Left (Annotation {
          annotated = Change {
            add = Nothing,
            remove = Just (Right (Composition {
              compositionName = "u",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (1, Just 2)}
              }))
            },
          annotation = DefiniteArticle
          }),
        option = AnyClassDiagram {
          anyClassNames = ["E", "C", "B", "D", "A"],
          anyRelationships = [
            Right (Inheritance {subClass = "E", superClass = "C"}),
            Right (Inheritance {subClass = "D", superClass = "A"}),
            Right (Composition {
              compositionName = "y",
              compositionPart =
                LimitedLinking {linking = "D", limits = (0, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "C", limits = (0, Just 1)}
              }),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart =
                LimitedLinking {linking = "B", limits = (0, Just 2)},
              aggregationWhole =
                LimitedLinking {linking = "A", limits = (1, Just 2)}
              }),
            Right (Association {
              associationName = "r",
              associationFrom =
                LimitedLinking {linking = "D", limits = (1, Just 1)},
              associationTo =
                LimitedLinking {linking = "B", limits = (2, Nothing)}
              }),
            Right (Composition {
              compositionName = "u",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (1, Just 2)}
              }),
            Right (Aggregation {
              aggregationName = "x",
              aggregationPart =
                LimitedLinking {linking = "A", limits = (0, Just 1)},
              aggregationWhole =
                LimitedLinking {linking = "C", limits = (1, Just 2)}
              })
            ]
          }
        }),
      (3, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object {isAnonymous = False, objectName = "c", objectClass = "C"},
            Object {isAnonymous = False, objectName = "d", objectClass = "D"},
            Object {isAnonymous = False, objectName = "b", objectClass = "B"},
            Object {isAnonymous = False, objectName = "e", objectClass = "E"},
            Object {isAnonymous = False, objectName = "a", objectClass = "A"}
            ],
          links = [
            Link {linkLabel = "z", linkFrom = "e", linkTo = "b"},
            Link {linkLabel = "t", linkFrom = "b", linkTo = "d"},
            Link {linkLabel = "x", linkFrom = "d", linkTo = "c"},
            Link {linkLabel = "y", linkFrom = "c", linkTo = "d"},
            Link {linkLabel = "x", linkFrom = "a", linkTo = "e"},
            Link {linkLabel = "v", linkFrom = "d", linkTo = "b"}
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["E", "A", "B", "C", "D"],
          anyRelationships = [
            Right (Composition {
              compositionName = "y",
              compositionPart =
                LimitedLinking {linking = "C", limits = (0, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "D", limits = (0, Just 1)}
              }),
            Right (Composition {
              compositionName = "z",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (0, Just 1)}
              }),
            Right (Association {
              associationName = "v",
              associationFrom =
                LimitedLinking {linking = "D", limits = (1, Just 1)},
              associationTo =
                LimitedLinking {linking = "B", limits = (1, Nothing)}
              }),
            Right (Inheritance {subClass = "D", superClass = "A"}),
            Right (Inheritance {subClass = "E", superClass = "C"}),
            Right (Aggregation {
              aggregationName = "x",
              aggregationPart =
                LimitedLinking {linking = "A", limits = (0, Just 1)},
              aggregationWhole =
                LimitedLinking {linking = "C", limits = (1, Just 2)}
              }),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart =
                LimitedLinking {linking = "B", limits = (0, Just 2)},
              aggregationWhole =
                LimitedLinking {linking = "A", limits = (1, Just 2)}
              })
            ]
          }
        }),
      (4, InValidOption {
        hint = Right (ObjectDiagram {
          objects = [
            Object {isAnonymous = False, objectName = "b", objectClass = "B"},
            Object {isAnonymous = False, objectName = "a", objectClass = "A"},
            Object {isAnonymous = False, objectName = "e", objectClass = "E"},
            Object {isAnonymous = False, objectName = "c", objectClass = "C"},
            Object {isAnonymous = False, objectName = "d", objectClass = "D"}
            ],
          links = [
            Link {linkLabel = "y", linkFrom = "d", linkTo = "c"},
            Link {linkLabel = "v", linkFrom = "d", linkTo = "b"},
            Link {linkLabel = "s", linkFrom = "e", linkTo = "b"},
            Link {linkLabel = "t", linkFrom = "b", linkTo = "d"},
            Link {linkLabel = "x", linkFrom = "d", linkTo = "c"},
            Link {linkLabel = "x", linkFrom = "a", linkTo = "e"}
            ]
          }),
        option = AnyClassDiagram {
          anyClassNames = ["C", "E", "B", "A", "D"],
          anyRelationships = [
            Right (Inheritance {subClass = "D", superClass = "A"}),
            Right (Aggregation {
              aggregationName = "t",
              aggregationPart =
                LimitedLinking {linking = "B", limits = (0, Just 2)},
              aggregationWhole =
                LimitedLinking {linking = "A", limits = (1, Just 2)}
              }),
            Right (Composition {
              compositionName = "s",
              compositionPart =
                LimitedLinking {linking = "E", limits = (1, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "B", limits = (0, Just 1)}
              }),
            Right (Aggregation {
              aggregationName = "x",
              aggregationPart =
                LimitedLinking {linking = "A", limits = (0, Just 1)},
              aggregationWhole =
                LimitedLinking {linking = "C", limits = (1, Just 2)}
              }),
            Right (Association {
              associationName = "v",
              associationFrom =
                LimitedLinking {linking = "D", limits = (1, Just 1)},
              associationTo =
                LimitedLinking {linking = "B", limits = (1, Nothing)}
              }),
            Right (Inheritance {
              subClass = "E",
              superClass = "C"
              }),
            Right (Composition {
              compositionName = "y",
              compositionPart =
                LimitedLinking {linking = "D", limits = (0, Nothing)},
              compositionWhole =
                LimitedLinking {linking = "C", limits = (0, Just 1)}
              })
            ]
          }
        })
      ],
    showExtendedFeedback = True,
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "Consider the following class diagram candidates:"),
          (German, "Betrachten Sie die folgenden Klassendiagrammkandidaten:")
          ])
        ],
      Special CdCandidates,
      Paragraph [
        Translated (listToFM [
          (English, "Which of these class diagram candidates are valid class diagrams?\nPlease state your answer by giving a list of numbers, indicating all valid class diagrams."),
          (German, "Welche dieser Klassendiagrammkandidaten sind valide Klassendiagramme?\nBitte geben Sie Ihre Antwort in Form einer Liste von Zahlen an, die alle gültigen Klassendiagramme enthält.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "For example,"),
          (German, "Zum Beispiel würde")
          ]),
        Code (listToFM [
          (English, "[1, 2]"),
          (German, "[1, 2]")
          ]),
        Translated (listToFM [
          (English, "would indicate that only class diagram candidates 1 and 2 of the given ones are valid class diagrams."),
          (German, "bedeuten, dass nur die Klassendiagrammkandidaten 1 und 2 der angegebenen Klassendiagrammkandidaten gültige Klassendiagramme sind.")
          ])
        ]
      ],
    addText = Nothing
    },
  allowLayoutMangling = True,
  shuffleNames = False,
  shuffleOptions = True
  }
