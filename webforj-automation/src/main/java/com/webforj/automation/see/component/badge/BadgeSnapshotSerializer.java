package com.webforj.automation.see.component.badge;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.badge.Badge;
import com.webforj.component.badge.BadgeExpanse;
import com.webforj.concern.HasText;
import java.util.Locale;
import java.util.Set;

/**
 * Serializes a badge with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class BadgeSnapshotSerializer implements SnapshotSerializer<Badge> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Badge> getContract() {
    return Badge.class;
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
  public void describe(Badge target, SnapshotNode node) {
    // a badge rests at small, so medium departs from it
    BadgeExpanse expanse = target.getExpanse();
    node.setAttribute("expanse",
        expanse == BadgeExpanse.SMALL ? null : expanse.name().toLowerCase(Locale.ROOT));
  }
}
