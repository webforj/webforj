package com.webforj.bbjship.assets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

final class IconsGeneratorTest {

  private static BufferedImage createImage(int width, int height, Color color) {
    BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        image.setRGB(x, y, color.getRGB());
      }
    }

    return image;
  }

  private static int getAlpha(BufferedImage image, int x, int y) {
    return new Color(image.getRGB(x, y), true).getAlpha();
  }

  @Nested
  class Icons {

    @Test
    void shouldCreateAnIconOfTheRequestedSize() {
      BufferedImage base = createImage(8, 8, Color.RED);

      BufferedImage icon = IconsGenerator.createIcon(base, 32, 24, 0f, Optional.empty());

      assertEquals(32, icon.getWidth());
      assertEquals(24, icon.getHeight());
      assertEquals(BufferedImage.TYPE_INT_ARGB, icon.getType());
    }

    @Test
    void shouldCenterTheBaseImageAndKeepItsAspectRatio() {
      BufferedImage base = createImage(8, 4, Color.RED);

      BufferedImage icon = IconsGenerator.createIcon(base, 20, 20, 0f, Optional.empty());

      assertEquals(0, getAlpha(icon, 10, 2));
      assertEquals(Color.RED.getRGB(), icon.getRGB(10, 10));
      assertEquals(0, getAlpha(icon, 10, 17));
    }

    @Test
    void shouldFillThePaddingWithTheBackgroundColor() {
      BufferedImage base = createImage(8, 8, Color.RED);

      BufferedImage icon = IconsGenerator.createIcon(base, 20, 20, .5f, Optional.of(Color.GREEN));

      assertEquals(Color.GREEN.getRGB(), icon.getRGB(1, 1));
      assertEquals(Color.RED.getRGB(), icon.getRGB(10, 10));
    }

    @Test
    void shouldLeaveThePaddingTransparentWithoutBackground() {
      BufferedImage base = createImage(8, 8, Color.RED);

      BufferedImage icon = IconsGenerator.createIcon(base, 20, 20, .5f, Optional.empty());

      assertEquals(0, getAlpha(icon, 1, 1));
      assertEquals(Color.RED.getRGB(), icon.getRGB(10, 10));
    }
  }

  @Nested
  class BackgroundColors {

    @ParameterizedTest
    @MethodSource("getHexadecimalColors")
    void shouldParseHexadecimalColors(String value) {
      BufferedImage image = createImage(8, 8, Color.RED);

      Optional<Color> color = IconsGenerator.resolveBackgroundColor(image, value);

      assertEquals(Optional.of(Color.GREEN), color);
    }

    @ParameterizedTest
    @MethodSource("getTransparentValues")
    void shouldAnswerNoColorForTransparentValues(String value) {
      BufferedImage image = createImage(8, 8, Color.RED);

      Optional<Color> color = IconsGenerator.resolveBackgroundColor(image, value);

      assertEquals(Optional.empty(), color);
    }

    @Test
    void shouldTakeTheCornerColorForAuto() {
      BufferedImage image = createImage(8, 8, Color.BLUE);

      Optional<Color> color = IconsGenerator.resolveBackgroundColor(image, "AUTO");

      assertEquals(Optional.of(Color.BLUE), color);
    }

    @Test
    void shouldRejectValueThatIsNotHexadecimal() {
      BufferedImage image = createImage(8, 8, Color.RED);
      String value = "green";

      assertThrows(NumberFormatException.class,
          () -> IconsGenerator.resolveBackgroundColor(image, value));
    }

    @Test
    void shouldAverageTheOpaquePixelsOfTheCorner() {
      BufferedImage image = createImage(10, 10, Color.GREEN);
      for (int x = 0; x < 5; x++) {
        for (int y = 0; y < 5; y++) {
          image.setRGB(x, y, x < 2 ? Color.RED.getRGB() : Color.BLUE.getRGB());
        }
      }

      image.setRGB(0, 0, 0);

      Optional<Color> color = IconsGenerator.findBackgroundColor(image);

      assertEquals(Optional.of(new Color(95, 0, 159)), color);
    }

    @Test
    void shouldSampleTheWholeOfAnImageSmallerThanTheCorner() {
      BufferedImage image = createImage(2, 2, Color.RED);

      Optional<Color> color = IconsGenerator.findBackgroundColor(image);

      assertEquals(Optional.of(Color.RED), color);
    }

    @Test
    void shouldFindNoColorForTransparentCorner() {
      BufferedImage image = new BufferedImage(8, 8, BufferedImage.TYPE_INT_ARGB);

      Optional<Color> color = IconsGenerator.findBackgroundColor(image);

      assertEquals(Optional.empty(), color);
    }

    private static Stream<String> getHexadecimalColors() {
      return Stream.of("00ff00", "#00ff00", "0x00ff00", "0f0", "#0F0");
    }

    private static Stream<Arguments> getTransparentValues() {
      return Stream.of("", "transparent", "TRANSPARENT").map(Arguments::of);
    }
  }
}
