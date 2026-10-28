package com.webforj.automation.see.component.icon;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.icons.Icon;
import com.webforj.component.icons.IconExpanse;
import java.util.Locale;

/**
 * Describes an icon, which rests at the extra small expanse.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class IconSnapshotSerializer implements SnapshotSerializer<Icon> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Icon> getContract() {
    return Icon.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(Icon target, SnapshotNode node) {
    // an icon rests at extra small, so medium departs from it
    IconExpanse expanse = target.getExpanse();
    node.setAttribute("expanse", expanse == IconExpanse.XSMALL || expanse == IconExpanse.NONE ? null
        : expanse.name().toLowerCase(Locale.ROOT));
  }
}
