package com.webforj.devtools.craftforj.source.site;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Node;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.stmt.ExpressionStmt;
import com.github.javaparser.ast.stmt.ReturnStmt;
import com.webforj.component.Component;
import com.webforj.component.ComponentSourceRegistry;
import com.webforj.component.ComponentSourceRegistry.SourceFrame;
import com.webforj.devtools.craftforj.source.SourceModificationException;
import com.webforj.devtools.craftforj.source.TargetResolver;
import com.webforj.devtools.craftforj.source.model.SourceLocation;
import com.webforj.devtools.craftforj.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.source.resolver.MethodResolver;
import com.webforj.devtools.craftforj.source.resolver.TypeResolver;
import com.webforj.devtools.craftforj.source.resolver.VariableResolver;
import com.webforj.devtools.craftforj.source.site.model.CreationSite;
import com.webforj.devtools.craftforj.utilities.ComponentTree;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Resolves the expression of the source that stands for one component of the running application.
 *
 * <p>
 * The expression is the creation of the component, or the call that asked for it when shared code
 * creates the component and returns it. A component that shares its expression with other
 * components, and one that another component builds as a part of itself, has no expression of its
 * own and is refused.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class ComponentSiteResolver {

  private static final String LAMBDA = "lambda$";

  private final SourceParserService parserService;
  private final TargetResolver targetResolver;
  private final Supplier<List<Component>> components;
  private final Function<Component, List<Component>> descendants;

  /**
   * Creates a resolver for the components the application shows.
   *
   * @param parserService the parser service
   */
  public ComponentSiteResolver(SourceParserService parserService) {
    this(parserService, ComponentTree::findAll, ComponentTree::findBelow);
  }

  /**
   * Creates a resolver.
   *
   * @param parserService the parser service
   * @param components supplies every component of the application
   * @param descendants finds the components a component holds
   */
  ComponentSiteResolver(SourceParserService parserService, Supplier<List<Component>> components,
      Function<Component, List<Component>> descendants) {
    this.parserService = parserService;
    this.targetResolver = new TargetResolver(parserService);
    this.components = components;
    this.descendants = descendants;
  }

  /**
   * Resolves the expressions that stand for a component and for what it holds.
   *
   * @param component the component
   *
   * @return the location of the component first, then the locations of the components below it that
   *         the same file creates by an expression of their own
   *
   * @throws SourceModificationException when the component has no expression of its own
   */
  public List<SourceLocation> resolveTree(Component component) {
    List<Component> all = components.get();
    SourceLocation root = resolveAmong(component, all);
    List<SourceLocation> locations = new ArrayList<>();
    locations.add(root);
    for (Component below : descendants.apply(component)) {
      try {
        SourceLocation location = resolveAmong(below, all);
        if (Path.of(location.getFile()).equals(Path.of(root.getFile()))) {
          locations.add(location);
        }
      } catch (SourceModificationException e) {
        // What has no expression of its own is written inside something else and goes with that
      }
    }

    return locations;
  }

  /**
   * Resolves the expression that stands for a component.
   *
   * @param component the component
   *
   * @return the location of the expression, with its creation site
   *
   * @throws SourceModificationException when the component has no expression of its own
   */
  SourceLocation resolve(Component component) {
    return resolveAmong(component, components.get());
  }

  private SourceLocation resolveAmong(Component component, List<Component> all) {
    List<SourceFrame> chain = ComponentSourceRegistry.getSourceFrames(component);
    String type = describe(component);
    if (chain.isEmpty()) {
      throw new SourceModificationException("The source of this " + type + " was not recorded");
    }

    long createdAt = ComponentSourceRegistry.getCreationTime(component);
    String called = null;
    for (int index = 0; index < chain.size(); index++) {
      SourceFrame frame = chain.get(index);
      String owner = frame.getSourcePoint().className();
      String file = targetResolver.resolveSourcePointFile(frame.getSourcePoint());
      if (file == null) {
        throw new SourceModificationException(type + " is created by " + getSimpleName(owner)
            + ", which has no Java source in this project");
      }

      CreationSite site = CompiledSites.find(frame, createdAt)
          .orElseThrow(() -> new SourceModificationException("The expression that creates this "
              + type + " was not recorded, reload the application"));
      // A method the compiler wrote stands between two the source declares and says nothing
      if (site.getKind() == CreationSite.Kind.BRIDGE) {
        continue;
      }

      if (site.getKind() == CreationSite.Kind.DELEGATION) {
        requireOwnConstructor(component, owner, called);
        continue;
      }

      requireProduct(component, site, called);
      requireSameCode(frame, all);
      Expression expression = SourceSites.find(parse(file), site);
      if (!isReturned(expression, site)) {
        requireAlone(component, chain.subList(0, index + 1), all);

        return createLocation(component, file, site, expression);
      }

      called = frame.getMethodName();
    }

    throw new SourceModificationException(
        type + " is returned by shared code, the call that asked for it cannot be found");
  }

  private CompilationUnit parse(String file) {
    try {
      return parserService.parse(Path.of(file)).orElseThrow(
          () -> new SourceModificationException("Failed to parse source file: " + file));
    } catch (IOException e) {
      throw new SourceModificationException("Failed to read source file: " + file);
    }
  }

  // The constructors of the class of the component run before the expression that asked for it
  private static void requireOwnConstructor(Component component, String owner, String called) {
    boolean own = TypeResolver.isInstance(component.getClass(), owner);
    if (!own || called != null) {
      throw new SourceModificationException(describe(component) + " is a part "
          + getSimpleName(owner) + " builds for itself, remove that instead");
    }
  }

  private static void requireProduct(Component component, CreationSite site, String called) {
    if (called != null) {
      // A lambda runs under the name of the method it stands for, which the call is known by
      boolean asked =
          called.startsWith(LAMBDA) ? isProduct(component, site) : called.equals(site.getName());
      if (site.getKind() != CreationSite.Kind.CALL || !asked) {
        throw new SourceModificationException(describe(component)
            + " is returned by shared code, the call that asked for it cannot be found");
      }

      return;
    }

    if (!isProduct(component, site)) {
      String owner =
          site.getKind() == CreationSite.Kind.CREATION ? getSimpleName(site.getProducedType())
              : site.getName() + "()";

      throw new SourceModificationException(
          describe(component) + " is a part " + owner + " builds for itself, remove that instead");
    }
  }

  // A creation produces the class it names, a call whatever fits the type it declares to return
  private static boolean isProduct(Component component, CreationSite site) {
    Class<?> type = component.getClass();
    if (site.getKind() == CreationSite.Kind.CREATION) {
      return TypeResolver.load(site.getProducedType(), type) == type;
    }

    return TypeResolver.isInstance(type, site.getProducedType());
  }

  private static boolean isReturned(Expression expression, CreationSite site) {
    Expression whole = MethodResolver.findChainEnd(expression);
    Node holder = whole.getParentNode().orElse(null);
    if (holder instanceof ReturnStmt) {
      return true;
    }

    // A lambda written as one expression returns it, unless the method it stands for returns
    // nothing
    if (holder instanceof ExpressionStmt body
        && body.getParentNode().orElse(null) instanceof LambdaExpr) {
      return !site.getDescriptor().endsWith(")V");
    }

    VariableDeclarator variable = VariableResolver.findHolder(whole);
    if (variable == null) {
      return false;
    }

    return VariableResolver.findReferences(variable).stream().map(MethodResolver::findChainEnd)
        .anyMatch(reference -> reference.getParentNode().orElse(null) instanceof ReturnStmt);
  }

  // Code that ran more than once built one component each time, from the one expression
  private static void requireAlone(Component component, List<SourceFrame> path,
      List<Component> all) {
    long twins =
        all.stream().filter(other -> other != component && other.getClass() == component.getClass())
            .map(ComponentSourceRegistry::getSourceFrames)
            .filter(chain -> chain.size() >= path.size() && isSame(chain, path)).count();
    if (twins > 0) {
      throw new SourceModificationException(describe(component) + " is created by code that built "
          + (twins + 1) + " components, its source stands for all of them");
    }
  }

  // A tool that rewrites classes while they load moves every instruction, what the running
  // method recorded for its other components then no longer fits the class file
  private static void requireSameCode(SourceFrame frame, List<Component> all) {
    for (Component other : all) {
      long createdAt = ComponentSourceRegistry.getCreationTime(other);
      for (SourceFrame recorded : ComponentSourceRegistry.getSourceFrames(other)) {
        if (isSameMethod(recorded, frame) && !CompiledSites.isKnown(recorded, createdAt)) {
          throw new SourceModificationException(getSimpleName(frame.getSourcePoint().className())
              + " does not run the code it was compiled to, reload the application");
        }
      }
    }
  }

  private static boolean isSame(List<SourceFrame> chain, List<SourceFrame> path) {
    for (int index = 0; index < path.size(); index++) {
      SourceFrame first = chain.get(index);
      SourceFrame second = path.get(index);
      if (first.getBytecodeIndex() != second.getBytecodeIndex() || !isSameMethod(first, second)) {
        return false;
      }
    }

    return true;
  }

  private static boolean isSameMethod(SourceFrame first, SourceFrame second) {
    return Objects.equals(first.getSourcePoint().className(), second.getSourcePoint().className())
        && Objects.equals(first.getMethodName(), second.getMethodName())
        && Objects.equals(first.getDescriptor(), second.getDescriptor());
  }

  private static SourceLocation createLocation(Component component, String file, CreationSite site,
      Expression expression) {
    VariableDeclarator variable =
        VariableResolver.findHolder(MethodResolver.findChainEnd(expression));
    Integer line = expression.getRange().map(range -> range.begin.line).orElse(null);
    SourceLocation location = new SourceLocation(file, line, site.getClassName(),
        variable == null ? null : variable.getNameAsString(), component.getClass().getName());
    location.setSite(site);

    return location;
  }

  private static String describe(Component component) {
    Class<?> type = component.getClass();
    while (type.getSimpleName().isEmpty() && type.getSuperclass() != null) {
      type = type.getSuperclass();
    }

    return type.getSimpleName();
  }

  private static String getSimpleName(String className) {
    String name = className.substring(className.lastIndexOf('.') + 1);

    return name.substring(name.lastIndexOf('$') + 1);
  }
}
