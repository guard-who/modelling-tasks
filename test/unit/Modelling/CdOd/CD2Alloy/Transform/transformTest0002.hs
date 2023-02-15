ClassDiagram {
  classNames = ["A", "B", "C", "D"],
  relationships = [
    Association {
      associationName = "x",
      associationFrom = LimitedLinking {
        linking = "A",
        limits = (0, Nothing)
        },
      associationTo = LimitedLinking {
        linking = "B",
        limits = (1, Just 2)
        }
      },
    Aggregation {
      aggregationName = "y",
      aggregationPart = LimitedLinking {
        linking = "D",
        limits = (0, Nothing)
        },
      aggregationWhole = LimitedLinking {
        linking = "C",
        limits = (1, Just 1)
        }
      }
    ]
  }
