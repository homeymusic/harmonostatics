\language "english"
\version "2.22.2"

\score {
  \new Staff {
    <c'' c'>1
    <c'' df'>1
    <c'' d'>1
    <c'' ef'>1
    <c'' e'>1
    <c'' f'>1
    <c'' gf'>1
    <c'' g'>1
    <c'' af'>1
    <c'' a'>1
    <c'' bf'>1
    <c'' b'>1
    <c'' c''>1
  }
}
\layout {
  \context {
    \Staff
    \omit TimeSignature
    \omit BarLine
    \omit Clef
  }
}

