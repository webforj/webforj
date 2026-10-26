package com.webforj.automation.see.component.slider;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.slider.Slider;
import com.webforj.concern.HasText;
import java.util.Set;

/**
 * Serializes a slider with its text and its vertical orientation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class SliderSnapshotSerializer implements SnapshotSerializer<Slider> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<Slider> getContract() {
    return Slider.class;
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
  public void describe(Slider target, SnapshotNode node) {
    if (target.getOrientation() == Slider.Orientation.VERTICAL) {
      node.addFlag("vertical");
    }
  }
}
