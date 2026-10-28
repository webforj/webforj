package com.webforj.automation.see.component.avatar;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.avatar.Avatar;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes an avatar with its text.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class AvatarSnapshotSerializer implements SnapshotSerializer<Avatar> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Avatar> getContract() {
    return Avatar.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Set<Class<?>> getServerHeldContracts() {
    return Set.of(HasText.class);
  }
}
