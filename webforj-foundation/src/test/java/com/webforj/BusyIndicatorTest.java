package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjBusyIndicator;
import com.basis.startup.type.BBjException;
import com.google.gson.Gson;
import com.webforj.component.Theme;
import com.webforj.component.spinner.DwcSpinner;
import com.webforj.component.spinner.SpinnerExpanse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.Mockito;

class BusyIndicatorTest {

  private BusyIndicator busyIndicator;
  private BBjBusyIndicator mockIndicator;

  @BeforeEach
  void setUp() {
    mockIndicator = Mockito.mock(BBjBusyIndicator.class);
    busyIndicator = new BusyIndicator(mockIndicator);
  }

  @Test
  void shouldSetAndGetText() {
    String message = "Loading...";
    busyIndicator.setText(message);
    verify(mockIndicator).setText(message);

    when(mockIndicator.getText()).thenReturn(message);
    assertEquals(message, busyIndicator.getText());
  }

  @Test
  void shouldSetAndGetHtml() {
    String html = "<div>Loading...</div>";
    busyIndicator.setHtml(html);
    verify(mockIndicator).setHtml(html);

    when(mockIndicator.getHtml()).thenReturn(html);
    assertEquals(html, busyIndicator.getHtml());
  }

  @Test
  void shouldSetAndGetBackdropVisible() throws BBjException {
    busyIndicator.setBackdropVisible(true);

    verify(mockIndicator, atLeast(1)).setAttribute("no-backdrop", "false");

    when(mockIndicator.getAttribute("no-backdrop")).thenReturn("false");
    assertTrue(busyIndicator.isBackdropVisible());
  }

  @Test
  void shouldSetGetVisible() {
    busyIndicator.setVisible(true);

    verify(mockIndicator).setVisible(true);

    when(mockIndicator.isVisible()).thenReturn(true);
    assertTrue(busyIndicator.isVisible());
  }

  @Test
  void shouldGetSpinner() {
    DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
    assertNotNull(spinner);
  }

  @Nested
  class SpinnerApi {
    @Test
    void shouldSpinnerSetAndGetVisible() throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      spinner.setVisible(true);
      verify(mockIndicator, atLeast(1)).setAttribute("suppress-spinner", "false");

      spinner.setVisible(false);
      verify(mockIndicator, atLeast(1)).setAttribute("suppress-spinner", "true");

      when(mockIndicator.getAttribute("suppress-spinner")).thenReturn("false");
      assertTrue(spinner.isVisible());

      when(mockIndicator.getAttribute("suppress-spinner")).thenReturn("true");
      assertFalse(spinner.isVisible());
    }

    @Test
    void shouldSpinnerSetAndGetClockwise() throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      spinner.setClockwise(true);
      verify(mockIndicator, atLeast(1)).setAttribute("spinner-clockwise", "true");

      spinner.setClockwise(false);
      verify(mockIndicator, atLeast(1)).setAttribute("spinner-clockwise", "false");

      when(mockIndicator.getAttribute("spinner-clockwise")).thenReturn("true");
      assertTrue(spinner.isClockwise());

      when(mockIndicator.getAttribute("spinner-clockwise")).thenReturn("false");
      assertFalse(spinner.isClockwise());
    }

    @ParameterizedTest
    @EnumSource(SpinnerExpanse.class)
    void shouldSpinnerSetAndGetExpanse(SpinnerExpanse expanse) throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      spinner.setExpanse(expanse);

      Gson gson = new Gson();
      String name = gson.fromJson(gson.toJson(expanse), String.class);
      verify(mockIndicator, atLeast(1)).setAttribute("spinner-expanse", name);

      when(mockIndicator.getAttribute("spinner-expanse")).thenReturn(name);
      assertEquals(expanse, spinner.getExpanse());
    }

    @Test
    void shouldSpinnerSetAndGetPaused() throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      spinner.setPaused(true);
      verify(mockIndicator).setAttribute("spinner-paused", "true");

      spinner.setPaused(false);
      verify(mockIndicator, atLeast(1)).setAttribute("spinner-paused", "false");

      when(mockIndicator.getAttribute("spinner-paused")).thenReturn("true");
      assertTrue(spinner.isPaused());

      when(mockIndicator.getAttribute("spinner-paused")).thenReturn("false");
      assertFalse(spinner.isPaused());
    }

    @Test
    void shouldSpinnerSetAndGetSpeed() throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      int speed = 1000;
      spinner.setSpeed(speed);
      verify(mockIndicator, atLeast(1)).setAttribute("spinner-speed", String.valueOf(speed));

      when(mockIndicator.getAttribute("spinner-speed")).thenReturn(String.valueOf(speed));
      assertEquals(speed, spinner.getSpeed());
    }

    @ParameterizedTest
    @EnumSource(Theme.class)
    void shouldSpinnerSetAndGetTheme(Theme theme) throws BBjException {
      DwcSpinner<BusyIndicator.BusyIndicatorSpinner> spinner = busyIndicator.getSpinner();
      spinner.setTheme(theme);

      Gson gson = new Gson();
      String name = gson.fromJson(gson.toJson(theme), String.class);

      verify(mockIndicator, atLeast(1)).setAttribute("spinner-theme", name);

      when(mockIndicator.getAttribute("spinner-theme")).thenReturn(name);
      assertEquals(theme, spinner.getTheme());
    }
  }
}

