package memnets.model

import memnets.utils.JavaUtils


inline infix fun <T : Dsl> T.having(block: T.() -> Unit): T {
    block()
    return this
}

fun Y.F(f: (Tick) -> Double): F {
    return createF("", f)
}

fun <T> T.asOption(): scala.Option<T> {
    return scala.Option.apply(this)
}

fun <T> Collection<T>.asScala(): scala.collection.Iterable<T> {
    return JavaUtils.asScala(this)
}


