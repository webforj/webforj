package com.webforj.devtools.craftforj.router;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.TypeDeclaration;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.ArrayInitializerExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MarkerAnnotationExpr;
import com.github.javaparser.ast.expr.Name;
import com.github.javaparser.ast.expr.SingleMemberAnnotationExpr;
import com.github.javaparser.ast.expr.StringLiteralExpr;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.inspector.source.parser.ImportWriter;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Writes route security annotations into route class sources.
 *
 * <p>
 * Replaces any existing security annotation on the route class with the requested one, managing the
 * matching imports. Formatting of untouched code is preserved.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class RouteSecurityModifier {

  private static final Map<SecurityAccess, String> ANNOTATIONS = createAnnotationMap();

  private final SourceParserService parserService;

  /** Creates a modifier with a default parser service. */
  public RouteSecurityModifier() {
    this(SourceParserService.getCurrent());
  }

  /**
   * Creates a modifier with a specific parser service.
   *
   * @param parserService the parser service to use
   */
  public RouteSecurityModifier(SourceParserService parserService) {
    this.parserService = parserService;
  }

  /**
   * Applies the given security access to the route class in the source file.
   *
   * @param file the absolute path to the Java source file
   * @param className the simple name of the route class
   * @param access the access to apply, {@link SecurityAccess#NONE} removes any security annotation
   * @param roles the allowed roles, required for {@link SecurityAccess#ROLES_ALLOWED}
   * @throws CraftforjActionException when the file cannot be parsed or the class is not found
   */
  public void apply(Path file, String className, SecurityAccess access, List<String> roles) {
    if (access == SecurityAccess.ROLES_ALLOWED && (roles == null || roles.isEmpty())) {
      throw new CraftforjActionException("At least one role is required for ROLES_ALLOWED");
    }

    CompilationUnit cu = parse(file);
    TypeDeclaration<?> type = findType(cu, className);

    removeSecurityAnnotations(type);
    if (access != SecurityAccess.NONE) {
      type.addAnnotation(createAnnotation(access, roles));
    }

    write(file,
        ImportWriter.sync(parserService.print(cu), ANNOTATIONS.values(), usedAnnotations(cu)));
  }

  private CompilationUnit parse(Path file) {
    try {
      return parserService.parseWithLexicalPreservation(file)
          .orElseThrow(() -> new CraftforjActionException("Failed to parse source file: " + file));
    } catch (IOException e) {
      throw new CraftforjActionException("Failed to read source file: " + file, e);
    }
  }

  private TypeDeclaration<?> findType(CompilationUnit cu, String className) {
    return cu.getTypes().stream().filter(type -> type.getNameAsString().equals(className))
        .findFirst().orElseThrow(
            () -> new CraftforjActionException("Class not found in source file: " + className));
  }

  private void removeSecurityAnnotations(TypeDeclaration<?> type) {
    List<AnnotationExpr> toRemove = type.getAnnotations().stream()
        .filter(annotation -> isSecurityAnnotation(annotation.getNameAsString())).toList();
    toRemove.forEach(AnnotationExpr::remove);
  }

  private Set<String> usedAnnotations(CompilationUnit cu) {
    return ANNOTATIONS.values().stream()
        .filter(qualifiedName -> cu.findAll(AnnotationExpr.class).stream()
            .anyMatch(annotation -> matchesName(annotation.getNameAsString(), qualifiedName)))
        .collect(Collectors.toSet());
  }

  private boolean isSecurityAnnotation(String name) {
    return ANNOTATIONS.values().stream()
        .anyMatch(qualifiedName -> matchesName(name, qualifiedName));
  }

  private boolean matchesName(String name, String qualifiedName) {
    String simpleName = qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1);
    return name.equals(simpleName) || name.equals(qualifiedName);
  }

  private AnnotationExpr createAnnotation(SecurityAccess access, List<String> roles) {
    String qualifiedName = ANNOTATIONS.get(access);
    Name name = new Name(qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1));

    if (access != SecurityAccess.ROLES_ALLOWED) {
      return new MarkerAnnotationExpr(name);
    }

    Expression value;
    if (roles.size() == 1) {
      value = roleLiteral(roles.get(0));
    } else {
      ArrayInitializerExpr array = new ArrayInitializerExpr();
      roles.forEach(role -> array.getValues().add(roleLiteral(role)));
      value = array;
    }

    return new SingleMemberAnnotationExpr(name, value);
  }

  private StringLiteralExpr roleLiteral(String role) {
    // setString escapes quotes and backslashes; the constructor writes them verbatim
    return new StringLiteralExpr().setString(role);
  }

  private void write(Path file, String content) {
    try {
      Files.writeString(file, content);
    } catch (IOException e) {
      throw new CraftforjActionException("Failed to write source file: " + file, e);
    }
  }

  private static Map<SecurityAccess, String> createAnnotationMap() {
    Map<SecurityAccess, String> map = new LinkedHashMap<>();
    map.put(SecurityAccess.PERMIT_ALL, "jakarta.annotation.security.PermitAll");
    map.put(SecurityAccess.DENY_ALL, "jakarta.annotation.security.DenyAll");
    map.put(SecurityAccess.ROLES_ALLOWED, "jakarta.annotation.security.RolesAllowed");
    map.put(SecurityAccess.ANONYMOUS, "com.webforj.router.security.annotation.AnonymousAccess");

    return map;
  }
}
