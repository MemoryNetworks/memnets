package memnets.model

import scala.beans.BeanProperty

/**
 * hints on visualization.
 * diff visualizations may or may not support all hints
 */
case class GridHints(
    @BeanProperty var scaleX: Double = 1.0,
    @BeanProperty var scaleY: Double = 1.0,
    @BeanProperty var opacity: Double = 0.8,
    @BeanProperty var fixedDimension: Boolean = true,
    @BeanProperty var width: Int = 512,
    @BeanProperty var height: Int = 512,
    @BeanProperty var showGlass: Boolean = false,
    @BeanProperty var showBorder: Boolean = true,
    @BeanProperty var reflection: Boolean = true,
    @BeanProperty var width3D: Int = 512,
    @BeanProperty var height3D: Int = 512,
    @BeanProperty var zoom3D: Double = 1.0
)
