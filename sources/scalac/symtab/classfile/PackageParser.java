/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import java.io.IOException;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;

import scala.tools.util.AbstractFile;
import scala.tools.util.Position;

import scalac.Global;
import scalac.symtab.Scope;
import scalac.symtab.SourceCompleter;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolLoader;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.SourceRepresentation;

/**
 * This class implements a package member loader. It can be used to
 * complete package class symbols.
 */
public class PackageParser extends SymbolLoader {

    //########################################################################
    // Private Fields

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public PackageParser(Global global) {
        super(global);
    }

    //########################################################################
    // Protected Methods

    /** Completes the package symbol by loading all its members. */
    protected String doComplete(Symbol peckage) {
	boolean isRoot = peckage.isRoot();
        String dirname = isRoot ? null
            : SourceRepresentation.externalizeFileName(peckage, "/");

        // collect JVM and source members
        HashMap sources = new HashMap();
        HashMap classes = new HashMap();
        HashSet packages = new HashSet();
        String[] base = global.classPath.components();
        for (int i = 0; i < base.length; i++) {
            AbstractFile dir = AbstractFile.open(base[i], dirname);
            if (dir == null) continue;
            try {
                String[] filenames = dir.list();
                if (filenames == null) continue;
                for (int j = 0; j < filenames.length; j++) {
                    String fname = filenames[j];
                    if (fname.endsWith("/") && !fname.equals("META-INF/")) {
                        String name = fname.substring(0, fname.length() - 1);
                        packages.add(name);
                        continue;
                    }
                    if (!isRoot && fname.endsWith(".class")) {
                        String name = fname.substring(0, fname.length() - 6);
                        if (!classes.containsKey(name))
                            classes.put(name, dir.open(fname));
                        continue;
                    }
                    if (!isRoot && fname.endsWith(".scala")) {
                        String name = fname.substring(0, fname.length() - 6);
                        if (!sources.containsKey(name))
                            sources.put(name, dir.open(fname));
                        continue;
                    }
                }
            } catch (IOException exception) {
                if (global.debug) exception.printStackTrace();
                continue;
            }
	}

        // create JVM and source members
        Scope members = new Scope();
        for (Iterator i = sources.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile sfile = (AbstractFile)entry.getValue();
            AbstractFile cfile = (AbstractFile)classes.remove(name);
            if (global.separate && cfile != null) {
                if (cfile.lastModified() > sfile.lastModified()) {
                    classes.put(name, cfile);
                    continue;
                }
            }
            packages.remove(name);
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new SourceCompleter(global, sfile);
            peckage.newLoadedClass(0, classname, loader, members);
        }
        for (Iterator i = classes.entrySet().iterator(); i.hasNext(); ) {
            HashMap.Entry entry = (HashMap.Entry)i.next();
            String name = (String)entry.getKey();
            AbstractFile cfile = (AbstractFile)entry.getValue();
            packages.remove(name);
            Name classname = Name.fromString(name).toTypeName();
            SymbolLoader loader = new ClassParser(global, cfile);
            peckage.newLoadedClass(JAVA, classname, loader, members);
        }
        for (Iterator i = packages.iterator(); i.hasNext(); ) {
            String name = (String)i.next();
            peckage.newLoadedPackage(Name.fromString(name), this, members);
        }

        // collect and create CLR members
	if (global.target == global.TARGET_MSIL)
            CLRPackageParser.instance(global).importCLRTypes(peckage, members);

        // initialize package
        peckage.setInfo(Type.compoundType(Type.EMPTY_ARRAY, members, peckage));
        return dirname == null ? "anonymous package" : "package '"+dirname+"'";
    }

    //########################################################################
}
