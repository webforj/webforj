package com.webforj.component.card;

import com.google.gson.annotations.SerializedName;
import com.webforj.component.Component;
import com.webforj.component.Expanse;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementCompositeContainer;
import com.webforj.component.element.PropertyDescriptor;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.component.element.concern.HasElementClickListener;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasVisibility;

/**
 * A card is a passive grouping surface for related content and actions.
 *
 * <p>
 * The card is built from optional regions. A figure holds an illustration, the header holds an
 * icon, a title, a caption and header actions, the body holds the main content, and the footer
 * closes the card. Every region is filled through its own slot, so a card can be as small as a body
 * or as rich as all regions together. A region whose slot has no content is not rendered at all.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@NodeName("dwc-card")
public class Card extends ElementCompositeContainer
    implements HasClassName<Card>, HasStyle<Card>, HasVisibility<Card>, HasSize<Card>,
    HasAttribute<Card>, HasExpanse<Card, Expanse>, HasElementClickListener<Card> {

  /**
   * The card elevations as defined by the Dwc Theme Engine shadow scale.
   */
  public enum Shadow {
    /** No shadow is applied. */
    @SerializedName("none")
    NONE,

    /** The xsmall shadow as defined by the Dwc Theme Engine. */
    @SerializedName("xs")
    XSMALL,

    /** The small shadow as defined by the Dwc Theme Engine. */
    @SerializedName("s")
    SMALL,

    /** The medium shadow as defined by the Dwc Theme Engine. */
    @SerializedName("m")
    MEDIUM,

    /** The large shadow as defined by the Dwc Theme Engine. */
    @SerializedName("l")
    LARGE,

    /** The xlarge shadow as defined by the Dwc Theme Engine. */
    @SerializedName("xl")
    XLARGE,

    /** The 2xlarge shadow as defined by the Dwc Theme Engine. */
    @SerializedName("2xl")
    XXLARGE;
  }

  /**
   * The card orientations.
   */
  public enum Orientation {
    /** The figure is stacked above the rest of the card's regions. */
    @SerializedName("vertical")
    VERTICAL,

    /** The figure is placed beside the rest of the card's regions. */
    @SerializedName("horizontal")
    HORIZONTAL;
  }

  // Slots
  private static final String FIGURE_SLOT = "figure";
  private static final String ICON_SLOT = "icon";
  private static final String TITLE_SLOT = "title";
  private static final String CAPTION_SLOT = "caption";
  private static final String HEADER_ACTIONS_SLOT = "header-actions";
  private static final String FOOTER_SLOT = "footer";

  // Property descriptors
  private final PropertyDescriptor<Shadow> shadowProp =
      PropertyDescriptor.property("shadow", Shadow.XSMALL);
  private final PropertyDescriptor<Expanse> expanseProp =
      PropertyDescriptor.property("expanse", Expanse.MEDIUM);
  private final PropertyDescriptor<Orientation> orientationProp =
      PropertyDescriptor.property("orientation", Orientation.VERTICAL);
  private final PropertyDescriptor<Boolean> dividedProp =
      PropertyDescriptor.property("divided", false);
  private final PropertyDescriptor<Boolean> borderlessProp =
      PropertyDescriptor.property("borderless", false);

  /**
   * Instantiates a new empty card.
   */
  public Card() {
    super();
  }

  /**
   * Instantiates a new card with the given body content.
   *
   * @param content the components to add to the card's body
   */
  public Card(Component... content) {
    this();
    add(content);
  }

  /**
   * Adds the given components to the card's figure slot.
   *
   * <p>
   * The figure holds the card's illustration, for instance an image, a video or a chart. When the
   * card's orientation is {@link Orientation#HORIZONTAL} the figure is placed beside the other
   * regions instead of above them. When nothing is added here the figure is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToFigure(Component... components) {
    getElement().add(FIGURE_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's icon slot.
   *
   * <p>
   * The icon is the leading visual of the header row and accepts any component, for instance an
   * icon or an avatar. When nothing is added here the icon is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToIcon(Component... components) {
    getElement().add(ICON_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's title slot.
   *
   * <p>
   * The title names the card and is also used as the card's accessible name, since the card
   * announces itself as a region. When nothing is added here the title is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToTitle(Component... components) {
    getElement().add(TITLE_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's caption slot.
   *
   * <p>
   * The caption is a short secondary line rendered under the title. When nothing is added here the
   * caption is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToCaption(Component... components) {
    getElement().add(CAPTION_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's header actions slot.
   *
   * <p>
   * The header actions sit at the end of the header row and typically hold buttons or a menu. When
   * nothing is added here the header actions are not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToHeaderActions(Component... components) {
    getElement().add(HEADER_ACTIONS_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's footer slot.
   *
   * <p>
   * The footer closes the card and typically holds actions or metadata. When nothing is added here
   * the footer is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToFooter(Component... components) {
    getElement().add(FOOTER_SLOT, components);
    return this;
  }

  /**
   * Adds the given components to the card's body.
   *
   * <p>
   * Alias for {@link #add(Component...)}. When nothing is added here the body is not rendered.
   * </p>
   *
   * @param components the components to add
   * @return the component itself
   */
  public Card addToBody(Component... components) {
    add(components);
    return this;
  }

  /**
   * Sets the card's elevation.
   *
   * <p>
   * The elevation is taken from the Dwc Theme Engine shadow scale. Combine {@link Shadow#NONE} with
   * a frame for an outlined card, a larger shadow with {@link #setBorderless(boolean)} for an
   * elevated card, and both for a flat card.
   * </p>
   *
   * @param shadow the elevation to apply
   * @return the component itself
   */
  public Card setShadow(Shadow shadow) {
    set(shadowProp, shadow);
    return this;
  }

  /**
   * Gets the card's elevation.
   *
   * @return the elevation
   */
  public Shadow getShadow() {
    return get(shadowProp);
  }

  /**
   * Sets the card's expanse.
   *
   * <p>
   * The expanse drives the card's padding, the gaps between its regions and the size of the title
   * and the caption.
   * </p>
   *
   * {@inheritDoc}
   */
  @Override
  public Card setExpanse(Expanse expanse) {
    set(expanseProp, expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Expanse getExpanse() {
    return get(expanseProp);
  }

  /**
   * Sets the card's orientation.
   *
   * <p>
   * A vertical card stacks the figure above the other regions. A horizontal card places the figure
   * beside them.
   * </p>
   *
   * @param orientation the orientation to apply
   * @return the component itself
   */
  public Card setOrientation(Orientation orientation) {
    set(orientationProp, orientation);
    return this;
  }

  /**
   * Gets the card's orientation.
   *
   * @return the orientation
   */
  public Orientation getOrientation() {
    return get(orientationProp);
  }

  /**
   * Sets whether the card draws dividers between its regions.
   *
   * <p>
   * When enabled a divider is drawn after the header and before the footer. Dividers of regions
   * which are not rendered are not drawn either.
   * </p>
   *
   * @param divided when {@code true} the dividers are drawn
   * @return the component itself
   */
  public Card setDivided(boolean divided) {
    set(dividedProp, divided);
    return this;
  }

  /**
   * Checks whether the card draws dividers between its regions.
   *
   * @return {@code true} when the dividers are drawn
   */
  public boolean isDivided() {
    return get(dividedProp);
  }

  /**
   * Sets whether the card is drawn without its frame ring.
   *
   * @param borderless when {@code true} the frame ring is not drawn
   * @return the component itself
   */
  public Card setBorderless(boolean borderless) {
    set(borderlessProp, borderless);
    return this;
  }

  /**
   * Checks whether the card is drawn without its frame ring.
   *
   * @return {@code true} when the frame ring is not drawn
   */
  public boolean isBorderless() {
    return get(borderlessProp);
  }

  Element getOriginalElement() {
    return getElement();
  }
}
