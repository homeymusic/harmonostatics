\language "english"
\version "2.22.2"

\score {
  \new Staff {
    <c'' c''>1
    <c'' cs''>1
    <c'' d''>1
    <c'' ds''>1
    <c'' e''>1
    <c'' f''>1
    <c'' fs''>1
    <c'' g''>1
    <c'' gs''>1
    <c'' a''>1
    <c'' as''>1
    <c'' b''>1
    <c'' c'''>1
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

