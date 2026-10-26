package com.webforj.automation.see;

import com.webforj.component.Component;
import com.webforj.component.window.Window;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasLabel;
import java.util.List;

final class Gauge extends Component implements HasLabel<Gauge>, HasFocusStatus {
  private final String unit;
  private final List<String> marks;
  private String label;
  private String zone;
  private int level;
  private int needle;
  private boolean focused;
  private Integer markCount;

  Gauge(String unit, String... marks) {
    this.unit = unit;
    this.marks = List.of(marks);
  }

  @Override
  public String getLabel() {
    return label;
  }

  @Override
  public Gauge setLabel(String label) {
    this.label = label;

    return this;
  }

  @Override
  public boolean hasFocus() {
    return focused;
  }

  String getUnit() {
    return unit;
  }

  List<String> getMarks() {
    return marks;
  }

  String getZone() {
    return zone;
  }

  Gauge setZone(String zone) {
    this.zone = zone;

    return this;
  }

  int getLevel() {
    return level;
  }

  Gauge setLevel(int level) {
    this.level = level;

    return this;
  }

  int getNeedle() {
    return needle;
  }

  Gauge setNeedle(int needle) {
    this.needle = needle;

    return this;
  }

  Gauge setFocused(boolean focused) {
    this.focused = focused;

    return this;
  }

  @Override
  protected void onCreate(Window window) {
    // the gauge adds nothing to the window it is attached to
  }

  @Override
  protected void onDestroy() {
    // the gauge holds nothing to release
  }

  Integer getMarkCount() {
    return markCount;
  }

  Gauge setMarkCount(int markCount) {
    this.markCount = markCount;

    return this;
  }
}
