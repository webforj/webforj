package com.webforj.automation.see;

import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.element.Element;
import com.webforj.component.element.ElementComposite;
import com.webforj.component.field.PasswordField;
import com.webforj.component.optiondialog.FileChooserFilter;
import com.webforj.concern.HasClientValidation;
import com.webforj.concern.HasEnablement;
import com.webforj.concern.HasExpanse;
import com.webforj.concern.HasFileChooserFilters;
import com.webforj.concern.HasFileDrop;
import com.webforj.concern.HasFocusStatus;
import com.webforj.concern.HasHelperText;
import com.webforj.concern.HasLabel;
import com.webforj.concern.HasMask;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMaxFiles;
import com.webforj.concern.HasMin;
import com.webforj.concern.HasPlaceholder;
import com.webforj.concern.HasPrefix;
import com.webforj.concern.HasStep;
import com.webforj.concern.HasSuffix;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTheme;
import com.webforj.concern.HasTooltip;
import com.webforj.concern.HasVisibility;
import com.webforj.data.concern.ReadOnlyAware;
import com.webforj.data.concern.RequiredAware;
import com.webforj.data.concern.ValueAware;
import com.webforj.environment.ObjectTable;
import com.webforj.router.RoutePathResolver;
import com.webforj.router.Router;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.temporal.TemporalAccessor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.regex.Pattern;

/**
 * Holds the snapshot serializers in the order they run. The serializers of the concerns run first,
 * then the serializers of the webforJ components, then the serializers registered by the
 * application.
 *
 * <p>
 * The state of each concern is held either on the server or on the client. A concern held on the
 * client is serialized only when the snapshot includes the browser state.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class SnapshotSerializerRegistry {
  private static final Logger logger = System.getLogger(SnapshotSerializerRegistry.class.getName());
  private static final Pattern GENERATED_NAME =
      Pattern.compile("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}");
  private static final String COMPONENT_SERIALIZER_PACKAGE =
      SnapshotSerializerRegistry.class.getPackageName() + ".component.";

  private final List<SnapshotSerializer<?>> serializers = new ArrayList<>();
  private final Set<SnapshotSerializer<?>> applicationSerializers = new HashSet<>();

  /**
   * Creates a registry with the given application serializers, which run after the serializers of
   * webforJ.
   *
   * @param applicationSerializers the serializers registered by the application, one per contract
   *
   * @throws IllegalStateException if two application serializers handle the same contract
   */
  public SnapshotSerializerRegistry(Collection<SnapshotSerializer<?>> applicationSerializers) {
    bindContracts();
    loadComponentSerializers();

    List<SnapshotSerializer<?>> sorted = new ArrayList<>(applicationSerializers);
    sorted.sort(Comparator.comparing(serializer -> serializer.getContract().getName()));
    Map<Class<?>, SnapshotSerializer<?>> byContract = new HashMap<>();
    for (SnapshotSerializer<?> serializer : sorted) {
      SnapshotSerializer<?> previous = byContract.putIfAbsent(serializer.getContract(), serializer);
      if (previous != null) {
        throw new IllegalStateException("The snapshot serializers " + previous.getClass().getName()
            + " and " + serializer.getClass().getName() + " both handle "
            + serializer.getContract().getName() + ". Register one serializer per contract.");
      }
    }

    this.applicationSerializers.addAll(sorted);
    serializers.addAll(sorted);
  }

  /**
   * Returns the registry of the current app, with the application serializers found on the
   * classpath.
   *
   * @return the current registry
   */
  static SnapshotSerializerRegistry getCurrent() {
    String key = SnapshotSerializerRegistry.class.getName();
    if (ObjectTable.contains(key)) {
      return (SnapshotSerializerRegistry) ObjectTable.get(key);
    }

    SnapshotSerializerRegistry registry =
        new SnapshotSerializerRegistry(loadApplicationSerializers());
    ObjectTable.put(key, registry);

    return registry;
  }

  /**
   * Returns the serializers of the given component in the order they run.
   *
   * @param component the component
   *
   * @return the serializers
   */
  public List<SnapshotSerializer<?>> forComponent(Component component) {
    List<SnapshotSerializer<?>> applicable = new ArrayList<>();
    for (SnapshotSerializer<?> serializer : serializers) {
      if (serializer.getContract().isInstance(component)) {
        applicable.add(serializer);
      }
    }

    return applicable;
  }

  /**
   * Checks if the given serializer was registered by the application.
   *
   * @param serializer the serializer
   *
   * @return true if the application registered the serializer
   */
  public boolean isApplicationSerializer(SnapshotSerializer<?> serializer) {
    return applicationSerializers.contains(serializer);
  }

  /**
   * Checks if the given serializer reads state held on the client for the given component. That is
   * the case for a concern held on the client that no serializer of the component returns from
   * {@link SnapshotSerializer#getServerHeldContracts()}, and for a concern a serializer of the
   * component returns from {@link SnapshotSerializer#getClientHeldContracts()}.
   *
   * @param serializer the serializer
   * @param component the component
   *
   * @return true if the serializer runs only when the snapshot includes the browser state
   */
  boolean isBrowserStateRequired(SnapshotSerializer<?> serializer, Component component) {
    if (!(serializer instanceof ContractSerializer<?> binding)) {
      return false;
    }

    if (binding.holder == Holder.CLIENT) {
      return !isContractHeldByServer(binding.contract, component);
    }

    return isContractHeldByClient(binding.contract, component);
  }

  /**
   * Checks if a serializer of the given component returns the given contract from
   * {@link SnapshotSerializer#getClientHeldContracts()}.
   *
   * @param contract the contract
   * @param component the component
   *
   * @return true if reading the contract from the component sends a request to the client
   */
  boolean isContractHeldByClient(Class<?> contract, Component component) {
    for (SnapshotSerializer<?> serializer : serializers) {
      if (serializer.getContract().isInstance(component)
          && serializer.getClientHeldContracts().contains(contract)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Checks if a serializer of the given component returns the given contract from
   * {@link SnapshotSerializer#getServerHeldContracts()}.
   *
   * @param contract the contract
   * @param component the component
   *
   * @return true if reading the contract from the component sends no request to the client
   */
  boolean isContractHeldByServer(Class<?> contract, Component component) {
    for (SnapshotSerializer<?> serializer : serializers) {
      if (serializer.getContract().isInstance(component)
          && serializer.getServerHeldContracts().contains(contract)) {
        return true;
      }
    }

    return false;
  }

  private void bindContracts() {
    bind(Element.class, Holder.SERVER,
        (element, node) -> node.setAttribute("node", element.getNodeName()));
    bind(HasText.class, Holder.CLIENT, this::describeText);
    bind(Component.class, Holder.SERVER, SnapshotSerializerRegistry::describeName);
    bind(Component.class, Holder.SERVER, SnapshotSerializerRegistry::describeRoute);
    bind(Component.class, Holder.SERVER, SnapshotSerializerRegistry::describeComposite);
    bind(HasLabel.class, Holder.SERVER,
        (hasLabel, node) -> node.setTextAttribute("label", hasLabel.getLabel()));
    bind(HasPlaceholder.class, Holder.SERVER, (hasPlaceholder, node) -> node
        .setAttribute("placeholder", hasPlaceholder.getPlaceholder()));
    bind(ValueAware.class, Holder.CLIENT, SnapshotSerializerRegistry::describeValue);
    bind(HasMin.class, Holder.SERVER, (min, node) -> node.setAttribute("min", min.getMin()));
    bind(HasMax.class, Holder.SERVER, (max, node) -> node.setAttribute("max", max.getMax()));
    bind(HasStep.class, Holder.SERVER, (step, node) -> node.setAttribute("step", step.getStep()));
    bind(HasMask.class, Holder.SERVER,
        (mask, node) -> node.setAttribute("mask", maskOf(mask.getMask())));
    bind(HasHelperText.class, Holder.SERVER,
        (helperText, node) -> node.setAttribute("helperText", helperText.getHelperText()));
    bind(HasTooltip.class, Holder.SERVER,
        (tooltip, node) -> node.setAttribute("tooltip", tooltip.getTooltipText()));
    bind(HasTheme.class, Holder.SERVER,
        (theme, node) -> node.setAttribute("theme", themeOf(theme.getTheme())));
    bind(HasExpanse.class, Holder.SERVER,
        (expanse, node) -> node.setAttribute("expanse", expanseOf(expanse.getExpanse())));
    bind(HasVisibility.class, Holder.SERVER,
        (hasVisibility, node) -> flag(node, "hidden", !hasVisibility.isVisible()));
    bind(HasEnablement.class, Holder.SERVER,
        (hasEnablement, node) -> flag(node, "disabled", !hasEnablement.isEnabled()));
    bind(ReadOnlyAware.class, Holder.SERVER,
        (readOnly, node) -> flag(node, "readonly", readOnly.isReadOnly()));
    bind(RequiredAware.class, Holder.SERVER,
        (required, node) -> flag(node, "required", required.isRequired()));
    bind(HasClientValidation.class, Holder.CLIENT, SnapshotSerializerRegistry::describeValidation);
    bind(HasFocusStatus.class, Holder.CLIENT,
        (focus, node) -> flag(node, "focused", focus.hasFocus()));
    bind(HasPrefix.class, Holder.SERVER,
        (prefix, node) -> part(node, prefix.getPrefixComponent(), "prefix"));
    bind(HasSuffix.class, Holder.SERVER,
        (suffix, node) -> part(node, suffix.getSuffixComponent(), "suffix"));
    bind(HasFileDrop.class, Holder.SERVER, (drop, node) -> flag(node, "drop", drop.isDrop()));
    bind(HasMaxFiles.class, Holder.SERVER,
        (maxFiles, node) -> node.setAttribute("maxFiles", maxFiles.getMaxFiles()));
    bind(HasFileChooserFilters.class, Holder.SERVER, SnapshotSerializerRegistry::describeFilters);
  }

  private void loadComponentSerializers() {
    List<SnapshotSerializer<?>> componentSerializers = new ArrayList<>();
    for (SnapshotSerializer<?> serializer : loadSerializers()) {
      if (isComponentSerializer(serializer)) {
        componentSerializers.add(serializer);
      }
    }

    componentSerializers.sort(Comparator.comparing(serializer -> serializer.getClass().getName()));
    serializers.addAll(componentSerializers);
  }

  private void describeText(HasText<?> hasText, SnapshotNode node) {
    boolean ownText = node.getChildren().stream().allMatch(child -> isPart(child))
        || isContractHeldByServer(HasText.class, node.getComponent());
    if (!isSecret(hasText) && ownText) {
      node.setText(hasText.getText());
    }
  }

  private static void describeName(Component component, SnapshotNode node) {
    String name = component.getName();
    boolean generated = GENERATED_NAME.matcher(String.valueOf(name)).matches();
    // the parent names the slot of a part it holds without an element, that name stays
    if (!node.hasAttribute("slot")) {
      node.setAttribute("slot", slotOf(component, node));
    }

    node.setTextAttribute("name", generated ? null : name);
  }

  private static void describeValue(ValueAware<?, ?> valueAware, SnapshotNode node) {
    if (isSecret(valueAware)) {
      return;
    }

    node.setTextAttribute("value", scalar(valueAware));
  }

  private static void describeValidation(HasClientValidation<?> validation, SnapshotNode node) {
    if (node.getComponent().isAttached() && validation.isInvalid()) {
      node.addFlag("invalid").setAttribute("message", validation.getInvalidMessage());
    }
  }

  private static void describeFilters(HasFileChooserFilters<?> filters, SnapshotNode node) {
    FileChooserFilter active = filters.getActiveFilter();
    for (FileChooserFilter filter : filters.getFilters()) {
      SnapshotNode line = node.addChild("Filter").setText(filter.getDescription())
          .setAttribute("pattern", filter.getPattern());
      if (active != null && active.getDescription().equals(filter.getDescription())) {
        line.addFlag("selected");
      }
    }
  }

  private <C> void bind(Class<C> contract, Holder holder, BiConsumer<C, SnapshotNode> reader) {
    serializers.add(new ContractSerializer<>(contract, holder, reader));
  }

  // a part fills a slot of its parent, named by the element or held as its prefix or suffix
  private static boolean isPart(SnapshotNode child) {
    Component component = child.getComponent();
    Component parent = child.getParent() == null ? null : child.getParent().getComponent();
    if (slotOf(component, child) != null) {
      return true;
    }

    try {
      return parent instanceof HasPrefix<?> prefix && prefix.getPrefixComponent() == component
          || parent instanceof HasSuffix<?> suffix && suffix.getSuffixComponent() == component;
    } catch (UnsupportedOperationException e) {
      return false;
    }
  }

  private static String slotOf(Component component, SnapshotNode node) {
    SnapshotNode parent = node.getParent();
    Element element = parent == null ? null : elementOf(parent.getComponent());
    if (element == null) {
      return null;
    }

    String slot = element.findComponentSlot(component);

    return slot == null || slot.isEmpty() ? null : slot;
  }

  private static Element elementOf(Component component) {
    if (component instanceof Element element) {
      return element;
    }

    if (component instanceof ElementComposite) {
      return (Element) ComponentUtil.getBoundComponent(component);
    }

    return null;
  }

  // the route the view is registered under, as declared, the frame carries the address. A router
  // also knows routes registered in code, a view without one still declares its own
  private static void describeRoute(Component component, SnapshotNode node) {
    Class<? extends Component> type = component.getClass();
    Router router = Router.getCurrent();
    Optional<String> registered =
        router == null ? Optional.empty() : router.getRegistry().getRouteByComponent(type);

    node.setAttribute("route", registered.orElseGet(() -> RoutePathResolver.resolvePath(type)));
  }

  // a composite stands for the chain of components it binds, down to the element at its end. The
  // line names what it binds directly and the node of that element, the chain has no lines of its
  // own
  private static void describeComposite(Component component, SnapshotNode node) {
    if (!node.isComposites() || !(component instanceof Composite<?>)) {
      return;
    }

    Component bound = ComponentUtil.getBoundComponent(component);
    node.setAttribute("composite", bound.getClass().getSimpleName());
    Component end = bound;
    while (end instanceof Composite<?>) {
      end = ComponentUtil.getBoundComponent(end);
    }

    if (end instanceof Element element) {
      node.setAttribute("node", element.getNodeName());
    }
  }

  private static String maskOf(String mask) {
    return mask == null || mask.chars().allMatch(c -> c == 'X') ? null : mask;
  }

  // medium is the resting size of most components and none leaves the element without an expanse
  // of its own, a component resting at another size skips it in its own serializer
  private static String expanseOf(Object expanse) {
    if (!(expanse instanceof Enum<?> value) || value.name().equals("MEDIUM")
        || value.name().equals("NONE")) {
      return null;
    }

    return value.name().toLowerCase(Locale.ROOT).replace('_', '-');
  }

  private static String themeOf(Object theme) {
    if (!(theme instanceof Enum<?> value) || value.name().equals("DEFAULT")) {
      return null;
    }

    return value.name().toLowerCase(Locale.ROOT).replace('_', '-');
  }

  private static void part(SnapshotNode node, Component component, String slot) {
    if (component == null) {
      return;
    }

    for (SnapshotNode child : node.getChildren()) {
      if (child.getComponent() == component) {
        child.setAttribute("slot", slot);
        return;
      }
    }

    node.addChild(component).setAttribute("slot", slot);
  }

  private static boolean isSecret(Object component) {
    return component instanceof PasswordField;
  }

  private static void flag(SnapshotNode node, String key, boolean set) {
    if (set) {
      node.addFlag(key);
    }
  }

  private static Object scalar(ValueAware<?, ?> valueAware) {
    Object value = valueAware.getValue();
    boolean scalar =
        value instanceof CharSequence || value instanceof Number || value instanceof Boolean
            || value instanceof Enum<?> || value instanceof TemporalAccessor;
    if (!scalar || Boolean.FALSE.equals(value)) {
      return null;
    }

    return value;
  }

  private static List<SnapshotSerializer<?>> loadApplicationSerializers() {
    List<SnapshotSerializer<?>> serializers = new ArrayList<>();
    for (SnapshotSerializer<?> serializer : loadSerializers()) {
      if (!isComponentSerializer(serializer)) {
        serializers.add(serializer);
      }
    }

    return serializers;
  }

  private static List<SnapshotSerializer<?>> loadSerializers() {
    List<SnapshotSerializer<?>> serializers = new ArrayList<>();
    Iterator<?> loaded = ServiceLoader.load(SnapshotSerializer.class).iterator();
    boolean more = true;
    while (more) {
      try {
        more = loaded.hasNext();
        if (more) {
          SnapshotSerializer<?> serializer = (SnapshotSerializer<?>) loaded.next();
          serializer.getContract();
          serializers.add(serializer);
        }
      } catch (ServiceConfigurationError | LinkageError e) {
        logger.log(Level.DEBUG,
            "A snapshot serializer of a component missing from the classpath was skipped", e);
      }
    }

    return serializers;
  }

  private static boolean isComponentSerializer(SnapshotSerializer<?> serializer) {
    return serializer.getClass().getPackageName().startsWith(COMPONENT_SERIALIZER_PACKAGE);
  }

  private enum Holder {
    SERVER, CLIENT
  }

  private static final class ContractSerializer<C> implements SnapshotSerializer<C> {
    private final Class<C> contract;
    private final Holder holder;
    private final BiConsumer<C, SnapshotNode> reader;

    private ContractSerializer(Class<C> contract, Holder holder,
        BiConsumer<C, SnapshotNode> reader) {
      this.contract = contract;
      this.holder = holder;
      this.reader = reader;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<C> getContract() {
      return contract;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void describe(C target, SnapshotNode node) {
      try {
        reader.accept(target, node);
      } catch (UnsupportedOperationException e) {
        logger.log(Level.DEBUG, "{0} declares {1} without supporting it",
            target.getClass().getName(), contract.getName());
      }
    }
  }
}
