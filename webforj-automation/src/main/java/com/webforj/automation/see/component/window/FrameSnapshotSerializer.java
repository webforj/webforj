package com.webforj.automation.see.component.window;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasText;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import java.util.Set;

/**
 * Serializes a frame with its text, and its current location when the snapshot includes the browser
 * state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class FrameSnapshotSerializer implements SnapshotSerializer<Frame> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Frame> getContract() {
    return Frame.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Frame target, SnapshotNode node) {
    if (!node.isBrowserState()) {
      return;
    }

    Router router = Router.getCurrent();
    if (router != null) {
      node.setAttribute("location",
          router.getHistory().getLocation().map(Location::getFullURI).orElse(null));
    }
  }
}
