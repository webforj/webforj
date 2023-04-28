package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjImage;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import org.dwcj.App;
import org.dwcj.component.button.Button;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.tree.Tree;
import org.dwcj.component.window.Frame;
import org.dwcj.component.window.Panel;
import org.dwcj.exceptions.DwcjException;
// import org.dwcj.utilities.ImageUtil;

public class Demo extends App {

  @Override
  public void run() throws DwcjException {
    Frame frame = new Frame();
    Panel p = new Panel();

    frame.add(p);

    Tree tree = new Tree(0, "Root");

    EventListener<MouseEnterEvent> mel = e -> {
      App.consoleLog("Mouse Enter");
    };


    tree.addNode(1, 0, "Build").addNode(2, 0, "Res").addNode(3, 0, "Lib");


    tree.addNode(10, 1, "Build1");
    tree.addNode(11, 1, "Build2");
    tree.addNode(12, 1, "Build3");


    tree.addNode(20, 2, "Res1");
    tree.addNode(21, 2, "Res2");
    tree.addNode(22, 2, "Res3");


    tree.addNode(30, 3, "Lib1");
    tree.addNode(31, 3, "Lib2");
    tree.addNode(32, 3, "Lib3");

    p.add(tree);
    tree.onMouseEnter(mel);

    // Button b = new Button("Image ?");
    // p.add(b);

    // b.setImage(new File("/Users/TimonGeisbauer/home.png"));
    // b.setImage("/Users/TimonGeisbauer/home.png");
  }
}
