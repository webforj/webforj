package com.webforj.automation.act.component.splitter;

import com.webforj.automation.act.UserInteraction;
import com.webforj.automation.act.exception.InteractionRejectedException;
import com.webforj.component.layout.splitter.Splitter;
import com.webforj.component.layout.splitter.event.SplitterResizeEvent;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Drags the handle of splitters within 0 to 100 percent.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SplitterInteraction {
  private static final double MINIMUM = 0;
  private static final double MAXIMUM = 100;

  private SplitterInteraction() {}

  /**
   * Returns the interaction that drags the handle of a splitter to the given position and lets go.
   * The splitter takes the position, then reports it.
   *
   * <pre>{@code
   * UserInteraction.perform(splitter, SplitterInteraction.resize(30))
   * }</pre>
   *
   * @param position the position the user lets go at, in percent
   *
   * @return the interaction to perform on the splitter
   * @throws InteractionRejectedException when performed, if the position is outside 0 to 100
   *         percent
   */
  public static Consumer<Splitter> resize(double position) {
    return splitter -> {
      if (position < MINIMUM || position > MAXIMUM) {
        throw new InteractionRejectedException(splitter,
            "ranges from 0 to 100 percent and a user cannot drag it to " + position);
      }

      splitter.setPositionRelative(position);

      Map<String, Object> data = new HashMap<>();
      data.put("positionRelative", position);
      data.put("isAdjusting", false);
      UserInteraction.fireEvent(new SplitterResizeEvent(splitter, data));
    };
  }
}
