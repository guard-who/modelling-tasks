ClassDiagram {
  classNames = ["A", "B", "C", "D"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    Composition {
      compositionName = "x",
      compositionPart = LimitedLinking {
        linking = "B",
        limits = (0, Nothing)
        },
      compositionWhole = LimitedLinking {
        linking = "C",
        limits = (0, Just 1)
        }
      },
    Composition {
      compositionName = "y",
      compositionPart = LimitedLinking {
        linking = "B",
        limits = (0, Nothing)
        },
      compositionWhole = LimitedLinking {
        linking = "D",
        limits = (0, Just 1)
        }
      }
    ]
  }
