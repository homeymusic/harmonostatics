\language "english"
\version "2.22.2"

\score {
  \new Staff {
    c'1\fermata
  }
}
\layout {
  \context {
    \Staff
    \omit TimeSignature
    \omit BarLine
  }
}

