\language "english"
\version "2.22.2"

\score {
  \new Staff {
    <c' c' c''>1
    <c' cs' c''>1
    <c' d' c''>1
    <c' ds' c''>1
    <c' e' c''>1
    <c' f' c''>1
    <c' fs' c''>1
    <c' g' c''>1
    <c' gs' c''>1
    <c' a' c''>1
    <c' as' c''>1
    <c' b' c''>1
    <c' c'' c''>1
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

