package com.webforj.automation.see.component.navigator;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.navigator.Navigator;
import com.webforj.concern.HasText;
import com.webforj.data.Paginator;
import java.util.Set;

/**
 * Serializes a navigator with its text, its current page, its page count and its page buttons, and
 * its navigation buttons when the snapshot includes the browser state.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class NavigatorSnapshotSerializer implements SnapshotSerializer<Navigator> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Navigator> getContract() {
    return Navigator.class;
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
  public void describe(Navigator navigator, SnapshotNode node) {
    Paginator paginator = navigator.getPaginator();
    node.setAttribute("page", paginator.getCurrent()).setAttribute("pages",
        paginator.getTotalPages());
    boolean moves = node.isBrowserState() && !navigator.isHideMainButtons();
    boolean first = paginator.getCurrent() <= 1;
    boolean last = paginator.getCurrent() >= paginator.getTotalPages();
    if (moves) {
      move(navigator, node, Navigator.Part.FIRST_BUTTON, "first", first);
      move(navigator, node, Navigator.Part.PREVIOUS_BUTTON, "previous", first);
    }

    if (navigator.getLayout() == Navigator.Layout.PAGES) {
      for (int page : paginator.getPages()) {
        SnapshotNode line = node.addChild("Page").setText(String.valueOf(page));
        if (page == paginator.getCurrent()) {
          line.addFlag("selected");
        }
      }
    }

    if (moves) {
      move(navigator, node, Navigator.Part.NEXT_BUTTON, "next", last);
      move(navigator, node, Navigator.Part.LAST_BUTTON, "last", last);
    }
  }

  private static void move(Navigator navigator, SnapshotNode node, Navigator.Part part, String name,
      boolean atEnd) {
    if (!navigator.isVisible(part)) {
      return;
    }

    SnapshotNode line = node.addChild("Move").setText(name);
    if (atEnd && navigator.isAutoDisable()) {
      line.addFlag("disabled");
    }
  }
}
