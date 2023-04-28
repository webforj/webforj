package org.dwcj;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import javax.imageio.ImageIO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/** Test for the ImageUtil. */
public class ImageUtilTest {
  @Test
  @DisplayName("Convert Bytes from a BBjImage to a Java Image")
  void convertionTest() {
    try {
      InputStream is = getClass().getResourceAsStream("bytes.txt");
      BufferedImage bi = ImageIO.read(is);
      assertEquals(BufferedImage.class, bi.getClass());
    } catch (IOException e) {
      e.fillInStackTrace();
    }
  }
}
