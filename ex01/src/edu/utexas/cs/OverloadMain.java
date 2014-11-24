package edu.utexas.cs;
/* Soot - a J*va Optimization Framework
 * Copyright (C) 2008 Eric Bodden
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import soot.Body;
import soot.BodyTransformer;
import soot.G;
import soot.Local;
import soot.PackManager;
import soot.Transform;
import soot.Unit;
import soot.jimple.DefinitionStmt;
import soot.jimple.GotoStmt;
import soot.jimple.IfStmt;
import soot.jimple.InstanceFieldRef;
import soot.jimple.IntConstant;
import soot.jimple.ReturnStmt;
import soot.toolkits.graph.ExceptionalUnitGraph;
import soot.util.cfgcmd.CFGToDotGraph;

import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import com.microsoft.z3.IntExpr;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;
import com.microsoft.z3.Z3Exception;

public class OverloadMain {

    public static void main(String[] args) {
        PackManager.v().getPack("jtp").add(
                                           new Transform("jtp.myTransform", new BodyTransformer() {

                                                   protected void internalTransform(Body body, String phase, Map options) {
                                                       String name = body.getMethod().getName();
                                                       ExceptionalUnitGraph eug = new ExceptionalUnitGraph(body);
                                                       ConsistencyAnalysis b = new ConsistencyAnalysis(eug);
                                                       List<Unit> heads = eug.getHeads();
                                                       try {
                                                       for (Iterator<Unit> i1 = heads.iterator(); i1.hasNext(); ) {
                                                           Unit u1 = i1.next();
                                                           AnalysisContainer conds = b.getFlowAfter(u1);
                                                           G.v().out.println("The flow at this head is " + conds.toString());
                                                           if (name.equals("compare")) {

                                                               String p0 = body.getParameterLocal(0).toString();
                                                               String p1 = body.getParameterLocal(1).toString();
                                                               IntExpr e0=b.m1.get(p0);
                                                               IntExpr e1=b.m1.get(p1);
                                                               IntExpr e[] = new IntExpr[3]; String o[] = new String[3];
                                                               for (int i2 = 0; i2 < 3; ++i2) {
                                                                   o[i2] = i2 < 1 ? "o": (o[i2 - 1] + "_"); while (b.m1.containsKey(o[i2])) o[i2] += "_";
                                                                   b.m1.put(o[i2], e[i2] = b.ctx.MkIntConst(o[i2]));
                                                               }
                                                               BoolExpr reflexCond = (BoolExpr) conds.eq0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[0]});
                                                               BoolExpr symmCond = b.ctx.MkIff(
                                                                                               (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                                               (BoolExpr) conds.gt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[0]}));
                                                               BoolExpr transCond = b.ctx.MkImplies(b.ctx.MkAnd(new BoolExpr[]
                                                                   {(BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[1]}),
                                                                    (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[1], e[2]})}),
                                                                                                    (BoolExpr) conds.lt0.Substitute(new Expr[] {e0, e1}, new Expr[] {e[0], e[2]}));
                                                               Solver solver = b.ctx.MkSolver();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(reflexCond));
                                                               G.v().out.println("reflexCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(symmCond));
                                                               G.v().out.println("symmCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               solver.Push(); solver.Assert(b.ctx.MkNot(transCond));
                                                               G.v().out.println("transCond is " + (solver.Check() == Status.UNSATISFIABLE ? "proved" : "not proved"));
                                                               solver.Pop();
                                                               
                                                           }
                                                       }
                                                       } catch (Z3Exception E) {
                                                           E.printStackTrace();
                                                       }

                                                     if (name.equals("compare")) {
                                                           G.v().out.println("Here's a compare body.");
                                                           for (Iterator<Unit> i1 = eug.iterator();
                                                                i1.hasNext();
                                                                ) {
                                                               Unit u1 = i1.next();
                                                               if (u1 instanceof IfStmt)
                                                                   G.v().out.println("Here's an if statement unit, with condition\n" +
                                                                                     ((IfStmt)u1).getCondition().toString() +
                                                                                     "\nand target\n" +
                                                                                     ((IfStmt)u1).getTarget().toString());
                                                               else if (u1 instanceof DefinitionStmt)
                                                                   G.v().out.println("Here's a definition statement unit, with left op " +
                                                                                     ((DefinitionStmt)u1).getLeftOp().toString() + ", which " +
                                                                                     (((DefinitionStmt)u1).getLeftOp() instanceof Local ? "is": "is not") +
                                                                                     " a local, and right op " +
                                                                                     ((DefinitionStmt)u1).getRightOp().toString() + ", which " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof IntConstant ? "is" : "is not") +
                                                                                     " an integer constant and " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof Local ? "is" : "is not")
                                                                                     + " a local and " +
                                                                                     (((DefinitionStmt)u1).getRightOp() instanceof InstanceFieldRef ? "is" : "is not") +
                                                                                     " an instance field reference.");
                                                               else if (u1 instanceof ReturnStmt)
                                                                   G.v().out.println("Here's a return statement unit, with op " +
                                                                                     ((ReturnStmt)u1).getOp().toString() + ", which " + (((ReturnStmt)u1).getOp() instanceof Local ? "is" : "is not") + " a local.");
                                                               else if (u1 instanceof GotoStmt)
                                                                   G.v().out.println("Here's a goto statement unit, with target " +
                                                                                     ((GotoStmt)u1).getTarget().toString());
                                                               else {
                                                                   G.v().out.println("Here's a unit.");
                                                                   G.v().out.println(u1.toString());
                                                               }
                                                               G.v().out.println("The flow at this unit is " + b.getFlowAfter(u1));
                                                           }
                                                           (new CFGToDotGraph()).drawCFG(eug, body).plot("/tmp/"+name+".dot");
                                                       } else if (false) {
                                                           G.v().out.println("Here's a body.");

                                                           for (Iterator<Unit> i1 = eug.iterator();
                                                                i1.hasNext();
                                                                ) {
                                                               Unit u1 = i1.next();
                                                               G.v().out.println("Here's a unit.");
                                                               G.v().out.println(u1.toString());

                                                           }
                                                       }

                                                       // use G.v().out instead of System.out so that Soot can
                                                       // redirect this output to the Eclipse console
                                                       G.v().out.println(body.getMethod());
                                                   }

                                               }));
		
        soot.Main.main(args);
    }
}
