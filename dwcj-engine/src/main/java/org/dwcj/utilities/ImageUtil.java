package org.dwcj.utilities;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.sysgui.BBjImage;
import com.basis.startup.type.BBjException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcjRuntimeException;

public class ImageUtil {

  private static BBjSysGui sysGui = Environment.getInstance().getSysGui();

  private ImageUtil() {}

  public static BBjImage convertBytestoBBjImage(byte[] bytes) {
    try {
      return sysGui.getImageManager().loadImageFromBytes(bytes);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to convert to BBjImage", e);
    }
  }

  public static BBjImage convertFileToBBjImage(File file) {
    try (FileInputStream is = new FileInputStream(file)) {
      return convertBytestoBBjImage(is.readAllBytes());
    } catch (IOException e) {
      throw new DwcjRuntimeException("Failed to convert to BBjImage", e);
    }
  }
}
