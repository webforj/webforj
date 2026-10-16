package com.webforj.bbjship.assets;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.util.Optional;

/**
 * Scales base icons with padding and background color.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class IconsGenerator {

  private static final String BG_AUTO = "auto";
  private static final String BG_TRANSPARENT = "transparent";

  private IconsGenerator() {}

  /**
   * Resolves the background color of an icon from its requested value.
   *
   * <p>
   * An empty value means transparent. The value {@code auto} takes the color of the image corner,
   * any other value is a hexadecimal RGB color with an optional {@code #} or {@code 0x} prefix.
   * </p>
   *
   * @param image the base image
   * @param backgroundColor the requested background color
   *
   * @return the background color, empty for a transparent background
   * @throws NumberFormatException if the value is not a hexadecimal color
   */
  static Optional<Color> resolveBackgroundColor(BufferedImage image, String backgroundColor)
      throws NumberFormatException {
    if (backgroundColor.isEmpty()) {
      backgroundColor = BG_TRANSPARENT;
    }

    if (BG_AUTO.equalsIgnoreCase(backgroundColor)) {
      return findBackgroundColor(image);
    } else if (BG_TRANSPARENT.equalsIgnoreCase(backgroundColor)) {
      return Optional.empty();
    } else {
      if (backgroundColor.startsWith("#")) {
        backgroundColor = backgroundColor.substring(1);
      } else if (backgroundColor.startsWith("0x")) {
        backgroundColor = backgroundColor.substring(2);
      }

      if (3 == backgroundColor.length()) {
        final char a = backgroundColor.charAt(0);
        final char b = backgroundColor.charAt(1);
        final char c = backgroundColor.charAt(2);
        final StringBuffer sb = new StringBuffer();
        sb.append(a);
        sb.append(a);
        sb.append(b);
        sb.append(b);
        sb.append(c);
        sb.append(c);
        backgroundColor = sb.toString();
      }

      final int rgb = Integer.parseInt(backgroundColor, 16);
      return Optional.of(new Color(rgb));
    }
  }

  /**
   * Creates an icon of the given size with the base image centered inside the padding.
   *
   * @param inputImage the base image
   * @param width the icon width
   * @param height the icon height
   * @param padding the padding ratio, 0 to 1
   * @param backgroundColor the background color, empty for a transparent background
   *
   * @return the icon
   */
  static BufferedImage createIcon(BufferedImage inputImage, int width, int height, float padding,
      Optional<Color> backgroundColor) {
    int innerWidth = (int) (width * (1 - padding));
    int innerHeight = (int) (height * (1 - padding));

    double inputAspect = (double) inputImage.getWidth() / inputImage.getHeight();
    if (innerWidth / (double) innerHeight > inputAspect) {
      innerWidth = (int) (innerHeight * inputAspect);
    } else {
      innerHeight = (int) (innerWidth / inputAspect);
    }

    int paddingX = (width - innerWidth) / 2;
    int paddingY = (height - innerHeight) / 2;

    BufferedImage outputImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    Graphics2D g2d = outputImage.createGraphics();

    if (backgroundColor.isPresent()) {
      g2d.setColor(backgroundColor.get());
      g2d.fillRect(0, 0, width, height);
    }

    Image resizedImage = inputImage.getScaledInstance(innerWidth, innerHeight, Image.SCALE_SMOOTH);

    g2d.drawImage(resizedImage, paddingX, paddingY, null);
    g2d.dispose();

    return outputImage;
  }

  /**
   * Finds the background color of an image from the opaque pixels of its top left corner.
   *
   * @param image the image
   *
   * @return the average color of the opaque corner pixels, empty when the corner is transparent
   */
  static Optional<Color> findBackgroundColor(BufferedImage image) {
    int regionSize = 5;
    int width = Math.min(regionSize, image.getWidth());
    int height = Math.min(regionSize, image.getHeight());

    long red = 0;
    long green = 0;
    long blue = 0;
    long count = 0;

    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        int rgb = image.getRGB(x, y);
        Color color = new Color(rgb, true);

        if (color.getAlpha() == 255) {
          red += color.getRed();
          green += color.getGreen();
          blue += color.getBlue();
          count++;
        }
      }
    }

    if (count == 0) {
      return Optional.empty();
    }

    int avgRed = (int) (red / count);
    int avgGreen = (int) (green / count);
    int avgBlue = (int) (blue / count);

    return Optional.of(new Color(avgRed, avgGreen, avgBlue));
  }
}
