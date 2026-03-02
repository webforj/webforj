package com.webforj.component.table.renderer;

import com.webforj.component.Theme;

/**
 * A renderer that displays the column's value function result with a percentage sign, optionally
 * with a mini {@code dwc-progressbar}. The column should return a numeric value.
 *
 * <pre>{@code
 * PercentageRenderer<MusicRecord> renderer = new PercentageRenderer<>();
 * renderer.setTheme(Theme.SUCCESS);
 *
 * table.addColumn("margin", r -> (int) (((r.getRetail() - r.getCost()) / r.getRetail()) * 100))
 *     .setRenderer(renderer);
 * }</pre>
 *
 * @param <T> the row data type
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public class PercentageRenderer<T> extends Renderer<T> {
  private boolean showProgressBar = true;
  private Theme theme;

  /**
   * Creates a new percentage renderer with the given theme and progress bar visibility.
   *
   * @param theme the theme to set
   * @param showProgressBar true to show a progress bar
   */
  public PercentageRenderer(Theme theme, boolean showProgressBar) {
    setTheme(theme);
    setShowProgressBar(showProgressBar);
  }

  /**
   * Creates a new percentage renderer with the given theme.
   *
   * @param theme the theme to set
   */
  public PercentageRenderer(Theme theme) {
    setTheme(theme);
  }

  /**
   * Creates a new percentage renderer.
   */
  public PercentageRenderer() {
    // default
  }

  /**
   * Sets whether to display a mini progress bar alongside the percentage text.
   *
   * @param showProgressBar true to show a progress bar
   * @return this renderer
   */
  public PercentageRenderer<T> setShowProgressBar(boolean showProgressBar) {
    this.showProgressBar = showProgressBar;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns whether the progress bar is shown.
   *
   * @return true if the progress bar is shown
   */
  public boolean isShowProgressBar() {
    return showProgressBar;
  }

  /**
   * Sets the theme of the progress bar.
   *
   * @param theme the theme to set
   * @return this renderer
   */
  public PercentageRenderer<T> setTheme(Theme theme) {
    this.theme = theme;
    fireChangeEvent();
    return this;
  }

  /**
   * Returns the theme of the progress bar.
   *
   * @return the theme
   */
  public Theme getTheme() {
    return theme;
  }

  /** {@inheritDoc} */
  @Override
  public String build() {
    if (!showProgressBar) {
      return "<span style='display:block;text-align:right;"
          + "font-variant-numeric:tabular-nums'><%= cell.value %>%</span>";
    }

    String themeAttr = theme != null ? " theme='" + toAttributeValue(theme) + "'" : "";

    return "<span style='display:flex;align-items:center;gap:8px;width:100%'>"
        + "<dwc-progressbar value='<%= cell.value %>' min='0' max='100'" + " text-visible='false'"
        + themeAttr + " style='flex:1;--dwc-progressbar-height:6px'></dwc-progressbar>"
        + "<span style='font-variant-numeric:tabular-nums;min-width:3.5em;text-align:right'>"
        + "<%= cell.value %>%</span></span>";
  }
}
