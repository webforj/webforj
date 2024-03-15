package com.webforj.component.layout.splitter.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.splitter.Splitter;
import java.util.Map;

/**
 * Emitted when the splitter panels are resized.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
@EventName(value = "dwc-splitter-resize")
@EventOptions(filter = "event.target.isSameNode(component)",
    data = {@EventData(key = "positionRelative", exp = "event.detail.positionRelative"),
        @EventData(key = "isAdjusting", exp = "event.detail.isAdjusting")})
public final class SplitterResizeEvent extends ComponentEvent<Splitter> {
  private double positionRelative;
  private boolean isAdjusting;

  /**
   * Creates a new splitter resize event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public SplitterResizeEvent(Splitter target, Map<String, Object> detail) {
    super(target, detail);
    positionRelative = ((Number) detail.get("positionRelative")).doubleValue();
    isAdjusting = (boolean) detail.get("isAdjusting");
  }

  /**
   * Gets the relative position of the splitter.
   *
   * @return the relative position of the splitter
   */
  public Double getPositionRelative() {
    return positionRelative;
  }

  /**
   * Gets whether the splitter is adjusting.
   *
   * @return true if the splitter is adjusting, false otherwise
   */
  public boolean isAdjusting() {
    return isAdjusting;
  }
}
