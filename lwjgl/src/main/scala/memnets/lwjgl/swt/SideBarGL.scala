package memnets.lwjgl.swt

import memnets.core._
import memnets.model._
import memnets.utils.SelectionModel
import org.eclipse.swt.SWT
import org.eclipse.swt.custom._
import org.eclipse.swt.events._
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets._

import scala.collection.mutable.ArrayBuffer

class SideBarGL(display: Display, engine: Engine) extends Logging {
  val shell = new Shell(display, SWT.CLOSE)
  import shell._
  setLayout(new FillLayout)
  addShellListener(new ShellAdapter {
    override def shellClosed(e: ShellEvent): Unit = { dispose() }
  })
  setSize(350, 350)
  setText("MemNets Sidebar")

  val tabbed = new CTabFolder(shell, SWT.NONE)

  def addTab(name: String, values: Iterable[String] = Seq()): List = {
    val tab = new CTabItem(tabbed, SWT.BORDER)
    val comp = new Composite(tabbed, SWT.None)
    comp.setLayout(new FillLayout)
    tab.setText(name)
    tab.setControl(comp)
    val list = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL)
    for (text <- values)
      list.add(text)
    list
  }
  def addTab2[T <: AnyRef](name: String, model: SelectionModel[T])(map: T => String): List = {
    val tab = new CTabItem(tabbed, SWT.BORDER)
    val comp = new Composite(tabbed, SWT.None)
    comp.setLayout(new FillLayout)
    tab.setText(name)
    tab.setControl(comp)
    val list = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL)

    model.onDirty = {
      logger.debug("model dirty")
      list.removeAll()
      for (item <- model.getItems)
        list.add(map(item))
    }
    model.addSelectionListener(sel => {
      logger.debug("model selected: " + sel)
      val i = list.getSelectionIndex
      val mi = model.getSelectedIndex
      if (i != mi) {
        list.select(mi)
      }
    })
    list.addSelectionListener(new SelectionAdapter {
      override def widgetSelected(e: SelectionEvent): Unit = {
        logger.debug("list selected: " + e)
        val i = list.getSelectionIndex
        val mi = model.getSelectedIndex
        if (i != mi)
          model.select(i)
      }
    })
    list
  }
  private val _builders = ArrayBuffer[BldType]()
  def addLibrary(lib: Library): Unit = {
    for (m <- lib.builders) {
      _builders += m
      builderList.add(m.name)
    }
  }
  val builderList = addTab("Library")
  builderList.addSelectionListener(new SelectionAdapter {
    override def widgetSelected(e: SelectionEvent): Unit = {
      val i = builderList.getSelectionIndex
      if (builderList.getItem(i) != engine.model.builder.name)
        engine.setBuilder(_builders(i))
    }
  })
  val trialList = addTab2("Trials", engine.trialsModel)(_.name)
  val paramList = addTab2("Params", engine.paramsModel)(_.name)
  val skinList = addTab2("Skins", engine.skinsModel)(_.name)
  val inputList = addTab2("Inputs", engine.inputModel)(_.description)

  def selectBuilder(bld: BldType): Unit = {
    if (bld != null) {
      val i = builderList.getSelectionIndex
      if (i < 0) {
        val i2 = builderList.getItems.indexOf(bld.name)
        builderList.select(i2)
      }
    }
  }
  def selectTrial(tri: Trial): Unit = {
    if (tri != null) {
      inputList.removeAll()
      for (input <- engine.inputModel.getItems)
        inputList.add(input.description)
    }
  }
  open()
}
