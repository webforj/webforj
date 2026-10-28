package com.webforj.automation.act;

import com.webforj.component.Component;
import com.webforj.environment.ObjectTable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * The interactions of a context, in two groups. The interactions the application registered are
 * asked first, then the interactions webforJ ships for its components and for the contracts a
 * component implements. The one interaction of a group that serves a component performs the verb.
 * An interaction registered for any component serves only the components no interaction of their
 * own class serves.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class ComponentInteractionRegistry {
  private static final Logger logger =
      System.getLogger(ComponentInteractionRegistry.class.getName());
  private static final String SHIPPED_INTERACTION_PACKAGE =
      ComponentInteractionRegistry.class.getPackageName() + ".";

  private final List<ComponentInteraction<?>> applicationInteractions = new ArrayList<>();
  private final List<ComponentInteraction<?>> shippedInteractions = new ArrayList<>();

  /**
   * Creates a registry that asks the given application interactions before the shipped ones.
   *
   * @param applicationInteractions the interactions the application registered
   *
   * @throws IllegalStateException if two application interactions perform the same verb for the
   *         same contract
   */
  ComponentInteractionRegistry(Collection<ComponentInteraction<?>> applicationInteractions) {
    List<ComponentInteraction<?>> sorted = new ArrayList<>(applicationInteractions);
    sorted.sort(Comparator.comparing(interaction -> interaction.getContract().getName()));
    Map<String, ComponentInteraction<?>> byVerb = new HashMap<>();
    for (ComponentInteraction<?> interaction : sorted) {
      for (Class<?> verb : findVerbs(interaction.getClass())) {
        String key = verb.getName() + " for " + interaction.getContract().getName();
        ComponentInteraction<?> previous = byVerb.putIfAbsent(key, interaction);
        if (previous != null) {
          throw new IllegalStateException("The interactions " + previous.getClass().getName()
              + " and " + interaction.getClass().getName() + " both perform " + verb.getSimpleName()
              + " for " + interaction.getContract().getName()
              + ". Register one interaction per verb and contract.");
        }
      }
    }

    this.applicationInteractions.addAll(sorted);
    loadShippedInteractions();
  }

  /**
   * Returns the registry of the current app, with the application interactions found on the
   * classpath.
   *
   * @return the current registry
   */
  static ComponentInteractionRegistry getCurrent() {
    String key = ComponentInteractionRegistry.class.getName();
    if (ObjectTable.contains(key)) {
      return (ComponentInteractionRegistry) ObjectTable.get(key);
    }

    ComponentInteractionRegistry registry =
        new ComponentInteractionRegistry(loadApplicationInteractions());
    ObjectTable.put(key, registry);

    return registry;
  }

  /**
   * Returns the interaction that performs the given verb on the given component.
   *
   * @param <I> the verb type
   * @param verb the interface of the verb
   * @param component the component
   *
   * @return the interaction, empty when no interaction performs the verb on the component
   *
   * @throws IllegalStateException if two interactions of the same group perform the verb on the
   *         component
   */
  <I extends ComponentInteraction<?>> Optional<I> find(Class<I> verb, Component component) {
    for (List<ComponentInteraction<?>> group : List.of(applicationInteractions,
        shippedInteractions)) {
      List<ComponentInteraction<?>> matches = group.stream()
          .filter(interaction -> verb.isInstance(interaction)
              && interaction.getContract().isInstance(component)
              && isSupported(interaction, component))
          .toList();
      List<ComponentInteraction<?>> ofTheClass =
          matches.stream().filter(found -> found.getContract() != Component.class).toList();
      if (!ofTheClass.isEmpty()) {
        matches = ofTheClass;
      }

      if (matches.size() > 1) {
        throw new IllegalStateException("The interactions " + matches.get(0).getClass().getName()
            + " and " + matches.get(1).getClass().getName() + " both perform "
            + verb.getSimpleName() + " for " + component.getClass().getSimpleName()
            + ". Register one interaction that serves the component.");
      }

      if (matches.size() == 1) {
        return Optional.of(verb.cast(matches.get(0)));
      }
    }

    return Optional.empty();
  }

  private void loadShippedInteractions() {
    List<ComponentInteraction<?>> interactions = new ArrayList<>();
    for (ComponentInteraction<?> interaction : loadInteractions()) {
      if (isShippedInteraction(interaction)) {
        interactions.add(interaction);
      }
    }

    interactions.sort(Comparator.comparing(interaction -> interaction.getClass().getName()));
    shippedInteractions.addAll(interactions);
  }

  @SuppressWarnings("unchecked")
  private static boolean isSupported(ComponentInteraction<?> interaction, Component component) {
    return ((ComponentInteraction<Object>) interaction).isSupported(component);
  }

  private static List<Class<?>> findVerbs(Class<?> type) {
    List<Class<?>> verbs = new ArrayList<>();
    for (Class<?> current = type; current != null; current = current.getSuperclass()) {
      for (Class<?> candidate : current.getInterfaces()) {
        if (ComponentInteraction.class.isAssignableFrom(candidate)
            && candidate != ComponentInteraction.class && !verbs.contains(candidate)) {
          verbs.add(candidate);
        }
      }
    }

    return verbs;
  }

  private static List<ComponentInteraction<?>> loadApplicationInteractions() {
    List<ComponentInteraction<?>> interactions = new ArrayList<>();
    for (ComponentInteraction<?> interaction : loadInteractions()) {
      if (!isShippedInteraction(interaction)) {
        interactions.add(interaction);
      }
    }

    return interactions;
  }

  private static List<ComponentInteraction<?>> loadInteractions() {
    List<ComponentInteraction<?>> interactions = new ArrayList<>();
    Iterator<?> loaded = ServiceLoader.load(ComponentInteraction.class).iterator();
    boolean more = true;
    while (more) {
      try {
        more = loaded.hasNext();
        if (more) {
          ComponentInteraction<?> interaction = (ComponentInteraction<?>) loaded.next();
          interaction.getContract();
          interactions.add(interaction);
        }
      } catch (ServiceConfigurationError | LinkageError e) {
        logger.log(Level.DEBUG,
            "An interaction of a component missing from the classpath was skipped", e);
      }
    }

    return interactions;
  }

  private static boolean isShippedInteraction(ComponentInteraction<?> interaction) {
    return interaction.getClass().getPackageName().startsWith(SHIPPED_INTERACTION_PACKAGE);
  }
}
