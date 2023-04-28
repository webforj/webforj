package org.dwcj.utilities;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.startup.type.BBjException;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import javax.imageio.ImageIO;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcjRuntimeException;

public class ImageUtil {

  private static BBjSysGui sysGui = Environment.getInstance().getSysGui();

  public static BBjImage convertImageToBBjImage(BufferedImage image, String format) {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    try {
      ImageIO.write(image, format, baos);
      return sysGui.getImageManager().loadImageFromBytes(baos.toByteArray());
    } catch (IOException | BBjException e) {
      throw new DwcjRuntimeException("Failed to convert Image", e);
    }
  }
}
