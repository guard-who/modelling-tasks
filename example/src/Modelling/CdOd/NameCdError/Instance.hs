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
      annotatedClasses = ["University Campus", "Building", "Facility Manager", "Person", "Room", "Professor"],
      annotatedRelationships = [
        Annotation {
          annotated = Right Association {
            associationName = "isResponsibleFor",
            associationFrom = LimitedLinking {
              linking = "Facility Manager",
              limits = (1, Nothing)
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
              limits = (0, Nothing)
              },
            compositionWhole = LimitedLinking {
              linking = "University Campus",
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
              limits = (1, Nothing)
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
            subClass = "Facility Manager",
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
        (English, "is correctly modelled"),
        (German, "ist vollständig richtig modelliert")
        ]))),
      ('d', (True, Custom (listToFM [
        (English, "does not satisfy a requirement of the scenario"),
        (German, "hält eine Vorgabe des Szenarios nicht ein")
        ])))
      ],
    showSolution = True,
    taskText = [
      Paragraph [
        Translated (listToFM [
          (English, "A Student received the following scenario:"),
          (German, "Eine Studentin hat folgendes Szenario erhalten:")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "A university campus consists of different buildings. Each building is cared for by a facility manager who is responsible for it. A facility manager is a person. Another kind of persons are the professors, who each have a specific room as their office. A building consists of different rooms, not each of these is a professors office."),
          (German, "Ein Universitätscampus besteht aus verschiedenen Gebäuden. Jedes Gebäude wird von einem Hausmeister betreut, der für es zuständig ist. Ein Hausmeister ist eine Person. Eine andere Art von Personen sind die Professoren, die jeweils einen bestimmten Raum als eigenes Büro haben. Ein Gebäude besteht aus verschiedenen Räumen, von denen nicht jedes ein Professorenbüro ist.")
          ])
        ],
      Paragraph [
        Translated (listToFM [
          (English, "She solved the task of creating a class diagram for the scenario at hand in the following way:"),
          (German, "Die Aufgabe, ein Klassendiagramm für dieses Szenario zu entwerfen hat sie folgendermaßen gelöst:")
          ])
        ],
      Paragraph [Special IncorrectCd],
      Paragraph [
        Translated (listToFM [
          (English, "Analyse and take a stand on the created class diagram regarding the original task!"),
          (German, "Analysieren Sie und beziehen Sie Stellung zum entworfenen Klassendiagramm hinsichtlich der ursprünglichen Aufgabe!")
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
          (English, "Name the specific relationships, that need to be changed in order to fix the issue, if such exist:"),
          (German, "Nennen Sie die konkreten Beziehungen, die geändert werden müssten, falls solche existieren:")
          ])
        ],
      Paragraph [Special RelationshipsList],
      Paragraph [
        Paragraph [
          Translated (listToFM [
            (English, "Please state your answer by providing: a number for your conclusion on the class diagram as well as (a potentialy empty) listing of numbers, indicating the relationships that would require fixing in your opinion. For instance"),
            (German, "Bitte geben Sie Ihre Antwort an, indem Sie folgendes angeben: eine Zahl für ihre Schlussfolgerung zum Klassendiagramm, und eine (eventuell leere) Auflistung von Zahlen für die Beziehungen, die Ihrer Meinung nach angepasst werden müssten. Zum Beispiel")
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
            (English, "would mean, that b is your conclusion on the class diagram and that relationships 3 and 4 would require fixing."),
            (German, "würde bedeuten, dass Ihre Folgerung zum Klassendiagramm b ist und dass die Beziehungen 3 und 4 angepasst werden müssten.")
            ])
          ]
        ]
      ]
    },
  allowLayoutMangling = True,
  shuffleNames = False
  }
