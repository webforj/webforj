package com.webforj.automation.see.component.button;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.Component;
import com.webforj.component.button.DwcButton;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a button with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class ButtonSnapshotSerializer implements SnapshotSerializer<DwcButton<?>> {

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("unchecked")
  public Class<DwcButton<?>> getContract() {
    return (Class<DwcButton<?>>) (Class<?>) DwcButton.class;
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
  public void describe(DwcButton<?> target, SnapshotNode node) {
    // the icon and the badge sit in slots of the button, not among its children
    addPart(node, target.getIcon(), null);
    addPart(node, target.getBadge(), "badge");
  }

  private static void addPart(SnapshotNode node, Component part, String slot) {
    if (part == null) {
      return;
    }

    for (SnapshotNode child : node.getChildren()) {
      if (child.getComponent() == part) {
        return;
      }
    }

    node.addChild(part).setAttribute("slot", slot);
  }
}
