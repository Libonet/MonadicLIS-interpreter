---
title: "TP3: Lenguaje Imperativo Simple Monadico"
author: Grillo (G-5811/4), Libonati (L-3256/5), Maiza (M-7116/1)
geometry: margin=2cm
output: pdf_document
header-includes:
  - \usepackage{graphicx}
  - \usepackage{ebproof}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

## EJERCICIO 1 a)

Vamos a demostrar que State es una mónada probando las tres leyes de mónadas para la instancia dada.

### Monad.1: $return~a >>= k = k~a$

$$return~x >>= f$$

$$=~<\text{return.1}>$$

$$State~(\lambda s \rightarrow (x~\text{:!:}~s)) >>= f$$

$$=~<(>>=).1>$$

$$
\begin{aligned}
State~(\lambda s'' \to\ &let (v~\text{:!:}~s') = runState~(State~(\lambda s \to\ (x~\text{:!:}~s)))~s'' \\
                               &in~runState~(f~v)~s')
\end{aligned}
$$

$$=~<\text{Lema 1: } (runState.State) = (State.runState) = id\text{, id.1, Def. (.)}>$$

$$
\begin{aligned}
State~(\lambda s'' \to\ &let (v~\text{:!:}~s') = (\lambda s \to\ (x~\text{:!:}~s))~s'' \\
                               &in~runState~(f~v)~s')
\end{aligned}
$$

$$=~<\text{Aplicación}>$$


$$
\begin{aligned}
State~(\lambda s'' \rightarrow &let~(v~\text{:!:}~s') = (x~\text{:!:}~s'') \\
                               &in~runState~(f~v)~s' )
\end{aligned}
$$

$$=~<\text{Def. let}>$$

$$State~(\lambda s'' \rightarrow runState~(f~x)~s'')$$

$$=~<\eta\text{-reducción: (}\lambda x \to\ f\ x) = f>$$

$$State~(runState~(f~x))$$

$$=~<\text{Lema 1, id.1, Def. (.)}>$$

$$f\ x$$


### Monad.2: $m >>= return = m$

$$(State~h) >>= return$$

$$=~<~(>>=).1~>$$

$$
\begin{aligned}
State (\lambda s \to\ &let (v\ :!:\ s') = runState (State\ h) s \\
                      &in\ runState (return\ v) s')
\end{aligned}
$$

$$\text{= < Lema 1: runState.State = State.runState = id >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ runState\ (return\ v)\ s')
\end{aligned}
$$

$$\text{= < def return >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ runState\ (State (\lambda s'' \to\ (v\ :!:\ s'')))\ s')
\end{aligned}
$$

$$\text{= < Lema 1 >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ (\lambda s'' \to\ (v\ :!:\ s'')) s')
\end{aligned}
$$

$$\text{= < App >}$$

$$
\begin{aligned}
State (\lambda s \to\ &let\ (v\ :!:\ s') = h\ s \\
                      &in\ (v\ :!:\ s'))
\end{aligned}
$$

$$\text{= < def let >}$$

$$State (\lambda s \to\ h\ s)$$

$$\text{= < }\eta\text{-reducción >}$$

$$\text{(State h)}$$

\newpage

### Monad.3: $m >>= (\lambda x~\to~k~x >>= h) = (m >>= k) >>= h$
