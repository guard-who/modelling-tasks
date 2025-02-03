ClassDiagram {
  classNames = ["A", "B", "C", "D"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    Composition {
      compositionName = "x",
      compositionPart = LimitedLinking {
        linking = "D",
        limits = (0, Nothing)
        },
      compositionWhole = LimitedLinking {
        linking = "C",
        limits = (1, Just 1)
        }
      }
    ]
  }
