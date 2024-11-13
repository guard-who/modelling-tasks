-- | 

module Modelling.CdOd.NameCdError.Instance where

import qualified Data.Map                         as M (fromList)

import Modelling.CdOd.NameCdError (
  NameCdErrorInstance (..),
  NameCdErrorTaskTextElement (..),
  Reason (..),
  Relevance (..),
  )

import Modelling.Auxiliary.Common       (ShuffleInstance (..))
import Modelling.CdOd.Types (
  Annotation (..),
  AnnotatedClassDiagram (..),
  CdDrawSettings (..),
  LimitedLinking (..),
  OmittedDefaultMultiplicities (..),
  Relationship (..),
  )

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
points: 0.15
-}
task2024_14 :: ShuffleInstance NameCdErrorInstance
task2024_14 = ShuffleInstance {
  taskInstance =   NameCdErrorInstance {
    byName = True,
    classDiagram = AnnotatedClassDiagram {
      annotatedClasses = ["UniversityCampus", "Building", "FacilityManager", "Person", "Room", "Professor"],
      annotatedRelationships = [
        Annotation {
          annotated = Right Association {
            associationName = "isResponsibleFor",
            associationFrom = LimitedLinking {
              linking = "FacilityManager",
              limits = (1, Just 1)
              },
            associationTo = LimitedLinking {
              linking = "Building",
              limits = (1, Nothing)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 1,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Association {
            associationName = "hasOfficeIn",
            associationFrom = LimitedLinking {
              linking = "Professor",
              limits = (1, Just 1)
              },
            associationTo = LimitedLinking {
              linking = "Room",
              limits = (1, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = True,
            listingPriority = 2,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Composition {
            compositionName = "isOn",
            compositionPart = LimitedLinking {
              linking = "Building",
              limits = (2, Nothing)
              },
            compositionWhole = LimitedLinking {
              linking = "UniversityCampus",
              limits = (0, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 3,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Composition {
            compositionName = "isPartOf",
            compositionPart = LimitedLinking {
              linking = "Room",
              limits = (2, Nothing)
              },
            compositionWhole = LimitedLinking {
              linking = "Building",
              limits = (1, Just 1)
              }
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 4,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Inheritance {
            subClass = "Professor",
            superClass = "Person"
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 5,
            referenceUsing = DefiniteArticle
            }
          },
        Annotation {
          annotated = Right Inheritance {
            subClass = "FacilityManager",
            superClass = "Person"
            },
          annotation = Relevant {
            contributingToProblem = False,
            listingPriority = 6,
            referenceUsing = DefiniteArticle
            }
          }
        ]
      },
    cdDrawSettings = CdDrawSettings {
      omittedDefaults = OmittedDefaultMultiplicities {
        aggregationWholeOmittedDefaultMultiplicity = Just (0, Nothing),
        associationOmittedDefaultMultiplicity = Just (0, Nothing),
        compositionWholeOmittedDefaultMultiplicity = Nothing
        },
      printNames = True,
      printNavigations = True
      },
    errorReasons = listToFM [
      ('a', (False, Custom (listToFM [
        (English, "contains a syntax error"),
        (German, "enthält einen Syntaxfehler")
        ]))),
      ('b', (False, Custom (listToFM [
        (English, "does not have any corresponding object diagram"),
        (German, "hat gar kein passendes Objektdiagramm")
        ]))),
      ('c', (False, Custom (listToFM [
        (English, "is fully correctly modelled"),
        (German, "ist vollständig richtig modelliert")
        ]))),
      ('d', (True, Custom (listToFM [
        (English, "does violate a requirement of the scenario"),
        (German, "hält eine Vorgabe des Szenarios nicht ein")
        ])))
      ],
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "A student received the following scenario:"),
          (German, "Ein Student hat folgendes Szenario erhalten:")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "A university campus consists of different buildings. Each building is cared for by a facility manager who is responsible for it. A facility manager is a person. Another kind of persons are the professors, who each have a specific room as own office. A building consists of different rooms, not each of which is a professor's office."),
          (German, "Ein Universitätscampus besteht aus verschiedenen Gebäuden. Jedes Gebäude wird von einem Hausmeister betreut, der für es zuständig ist. Ein Hausmeister ist eine Person. Eine andere Art von Personen sind die Professoren, die jeweils einen bestimmten Raum als eigenes Büro haben. Ein Gebäude besteht aus verschiedenen Räumen, von denen nicht jeder ein Professorenbüro ist.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "He solved the task of creating a class diagram for this scenario in the following way:"),
          (German, "Die Aufgabe, ein Klassendiagramm für dieses Szenario zu entwerfen, hat er folgendermaßen gelöst:")
          ])
        ],
      Paragraph [Special IncorrectCd],
      Paragraph [
        Translated (listToFM [
          (English, "Analyse and take a stance on the created class diagram regarding the scenario task of the student!"),
          (German, "Analysieren Sie und beziehen Sie Stellung zum entworfenen Klassendiagramm hinsichtlich der Szenario-Aufgabe des Studenten!")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "The class diagram ..."),
          (German, "Das Klassendiagramm ...")
          ])
        ],
      Paragraph [Special ReasonsList],
      Paragraph [
        Translated (listToFM [
          (English, "Name the specific relationships that would need to be changed on occasion:"),
          (German, "Nennen Sie die konkreten Beziehungen, die gegebenenfalls geändert werden müssten:")
          ])
        ],
      Paragraph [Special RelationshipsList],
      Paragraph [
        Paragraph [
          Translated (listToFM [
            (English, "Please state your answer by providing a letter for your conclusion about the class diagram as well as a listing of numbers for those relationships that would need to be adapted in your opinion. For example,"),
            (German, "Bitte geben Sie Ihre Antwort an, indem Sie Folgendes angeben: einen Buchstaben für Ihre Schlussfolgerung zum Klassendiagramm, und eine Auflistung von Zahlen für diejenigen Beziehungen, die Ihrer Meinung nach angepasst werden müssten. Zum Beispiel würde")
            ])
          ],
        Paragraph [
          Code (listToFM [
            (English, "due-to:\n- 3\n- 4\nreason: b\n"),
            (German, "due-to:\n- 3\n- 4\nreason: b\n")
            ])
          ],
        Paragraph [
          Translated (listToFM [
            (English, "would indicate that b is your conclusion about the class diagram and that the 3. and 4. relationship would need to be adapted."),
            (German, "bedeuten, dass b Ihre Schlussfolgerung zum Klassendiagramm ist und dass die 3. und 4. Beziehung angepasst werden müssten.")
            ])
          ]
        ]
      ]
    },
  allowLayoutMangling = True,
  shuffleNames = False
  }
