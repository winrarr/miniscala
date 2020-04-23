    {
    class C() {
      val a: Boolean = false
    };
      {
        {
          var x: C = new C();
          class C() {
            val a: Int = 42
          };
          {
            val y: C = x
          }
        }
      }
    }