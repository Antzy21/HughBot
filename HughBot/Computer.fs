namespace HughBot

open Chess

type Computer =
    {
        evaluationFunction: game -> move option * float;
    }