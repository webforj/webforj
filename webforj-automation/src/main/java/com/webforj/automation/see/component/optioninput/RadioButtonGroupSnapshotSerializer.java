package com.webforj.automation.see.component.optioninput;

import com.google.auto.service.AutoService;
import com.webforj.automation.see.SnapshotNode;
import com.webforj.automation.see.SnapshotSerializer;
import com.webforj.component.optioninput.RadioButton;
import com.webforj.component.optioninput.RadioButtonGroup;
import java.util.List;

/**
 * Serializes a radio button group with its radio buttons as children.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@AutoService(SnapshotSerializer.class)
public final class RadioButtonGroupSnapshotSerializer
    implements SnapshotSerializer<RadioButtonGroup> {

  /**
   * {@inheritDoc}
   */
  @Override
  public Class<RadioButtonGroup> getContract() {
    return RadioButtonGroup.class;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void describe(RadioButtonGroup group, SnapshotNode node) {
    List<SnapshotNode> siblings =
        node.getParent() == null ? List.of() : node.getParent().getChildren();
    for (RadioButton button : group.getButtons()) {
      SnapshotNode sibling = siblings.stream()
          .filter(candidate -> candidate.getComponent() == button).findFirst().orElse(null);
      if (sibling != null) {
        siblings.remove(sibling);
        node.getChildren().add(sibling);
      } else {
        node.addChild(button);
      }
    }
  }
}
