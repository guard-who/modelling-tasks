ClassDiagram {
  classNames = ["A", "B", "C", "D"],
  relationships = [
    Inheritance {subClass = "A", superClass = "B"},
    Composition {
      compositionName = "x",
      compositionPart = LimitedConnector {
        connectTo = "D",
        limits = (0, Nothing)
        },
      compositionWhole = LimitedConnector {
        connectTo = "C",
        limits = (1, Just 1)
        }
      }
    ]
  }
