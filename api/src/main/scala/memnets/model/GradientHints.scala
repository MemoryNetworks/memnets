package memnets.model

import scala.beans.BeanProperty

case class GradientHints(
    @BeanProperty var minDivs: Int = 10,
    @BeanProperty var hue: Float = 20.0f,
    // NOTE: keep spectrum float due to calculation
    @BeanProperty var spectrum: Float = 320.0f,
    @BeanProperty var maxLength: Int = Integer.MAX_VALUE,
    @BeanProperty var saturation: Float = 0.9f,
    @BeanProperty var brightness: Float = 1.0f,
    @BeanProperty var opacity: Float = 1.0f
)
