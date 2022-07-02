\language "english"
\version "2.22.2"
\header {
  title = "May Your Soul Rest"
  composer = "Brian McAuliff Mulloy"
}

\layout {
  indent = 0.0
  \context {
    \Score
    \omit BarNumber
  }
}
musicOne = \relative {
  \tempo 4 = 110
  \key c \phrygian
  f'8. f16 ef4 f4 c4 df2
  f8. f16 ef4 f4 bf,4 c2
  f8. f16 f,4 af4 c4 df4 ef4
  g,8. g16 af4 c4 bf4 c2~ c4
}
verseOne = \lyricmode {
May your soul rest in peace.
May your soul rest in peace.
May your soul rest, dear Pad -- dy.
May your soul rest in peace.
}  

\Score {
  <<
    \new Voice = "one" {
      \time 2/4
      \musicOne
    }
    \new Lyrics \lyricsto "one" {
      \verseOne
    }
  >>
}