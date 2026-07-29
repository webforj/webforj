package com.webforj.devtools.craftforj.inspector.contribution.layout.flexlayout.item;

import com.google.auto.service.AutoService;
import com.webforj.component.Component;
import com.webforj.concern.HasStyle;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.model.FeatureProperty;

/**
 * Contribution for the basis of a flex item, applied via {@code FlexLayout.setItemBasis}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@AutoService(FeatureHandler.class)
public class FlexItemBasisContribution extends FlexItemContribution {

  /** Creates the flex item basis contribution. */
  @SuppressWarnings({"unchecked", "rawtypes"})
  public FlexItemBasisContribution() {
    super("flex-basis", "Basis", "setItemBasis");

    setBuilderConfig(FeatureProperty.Builder::size);
    setGetter(this::readStyle);
    setSetter((parent, item, v) -> parent.setItemBasis(v == null ? null : String.valueOf(v),
        (Component & HasStyle) item));
    setResetter((parent, item) -> parent.setItemBasis(null, (Component & HasStyle) item));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected ValueExpression toSourceExpression(FeatureProperty property) {
    return scalarExpression(property);
  }
}
