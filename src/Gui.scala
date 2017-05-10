import java.awt._
import java.awt.event.{ActionEvent, ActionListener, KeyEvent, KeyListener}
import javax.swing.{JFrame, JPanel, Timer}

/**
  * Created by fcusumano on 5/9/17.
  */
object Gui extends App {

  class Canvas extends JPanel with ActionListener with KeyListener {
    private val timer = new Timer(1000/60, this)

    private var bufferImage: Image = _
    private var counter: Int = 0

    timer.start()

    def render(g: Graphics): Unit = {
      g.setColor(Color.BLACK)
      g.drawString(s"Counter: $counter", 100, 100)

      counter += 1
    }

    override def paint(g: Graphics): Unit = {
      val dimension = getSize()

      if(bufferImage == null) {
        bufferImage = createImage(dimension.width, dimension.height)
      }

      val bufferGraphics = bufferImage.getGraphics

      bufferGraphics.setColor(Color.WHITE)
      bufferGraphics.fillRect(0, 0, dimension.width, dimension.height)

      render(bufferGraphics)
      g.drawImage(bufferImage, 0, 0, null)
    }

    override def actionPerformed(actionEvent: ActionEvent): Unit = {
      repaint()
    }

    override def keyPressed(keyEvent: KeyEvent): Unit = {
    }

    override def keyTyped(keyEvent: KeyEvent): Unit = {}

    override def keyReleased(keyEvent: KeyEvent): Unit = {}
  }

  val frame = new JFrame("Nes Emulator")
  val canvas = new Canvas

  val ScalingFactor = 1

  frame.addKeyListener(canvas)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setLocationRelativeTo(null)
  frame.setResizable(false)
  frame.setPreferredSize(new Dimension(Ppu.ScreenWidth * ScalingFactor, Ppu.ScreenHeight * ScalingFactor))
  frame.setBackground(Color.WHITE)
  frame.add(canvas)
  frame.pack()
  frame.setVisible(true)

  while(true) {
  }
}
