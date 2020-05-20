(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics ;;
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
(* also uses Utilities *)

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect
                  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Registry.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Registry.register (self :> thing_type)

    method update : unit =
      self#move
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.  
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump
                     
    method! update =
      super#update;
      let posx, posy = self#pos in
      let infectiousness_total =
        (* calculate total infectiousness of all neighbors *)
        Utilities.sum_float
	  (List.map (fun obj -> obj#infectiousness)
                    (Registry.neighbors (self :> thing_type))) in
      if Utilities.flip_coin infectiousness_total then
        (* infected, so update the registry by replacing this object
           with an infected one *)
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new infected posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object 
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
            as super

    val mutable infected_counter = Counter.counter

    initializer
      Stat.infected#bump

    method! update =
      super#update;
      let posx, posy = self#pos in
      let infectiousness_total =

      (* use flip coin to check if they are dead. CMortality is the 
      probability someone dies*)

      if Utilites.flip_coin (cMortality) then 
        begin
          Stat.infected#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new deceased posx posy) :> thing_type)
        end
      else if float_of_int(infected_counter#count) >= Utilities.gaussian(fst cRECOVERY_PERIOD snd cRECOVERY_PERIOD) then
         begin 
          Stat.infected#debump
          Registry.deregister (self :> thing_type);
          Registry.register ((new recovered posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_INFECTED
    end


and deceased (initx : int) (inity : int) =
  object
    inherit person initx inity
                   cSTEP_SIZE_DECEASED
                   cINFECTIOUSNESS_DECEASED

    initializer
      Stat.deceased#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_DECEASED  
  
  end

and recovered (initx : int) (inity : int) =
  object
    inherit person initx inity
                   cSTEP_SIZE_RECOVERED
                   cINFECTIOUSNESS_RECOVERED
            as super

    val mutable recovered_counter = Counter.counter

    initializer
      Stat.recovered#bump

    method! update = 
      super#update ;
      recovered_counter#bump;
      if float_of_int(recoverd_counter#count) >= Utilities.gaussian (fst cImmunity_Period snd cImmunity_Period) then
          begin
            Stat.recoverede#debump;
            Registry.deregister (self :> thing_type);
            Registry.register ((new susceptible posx posy) :> thing_type)
          end      

      method! draw =
        let x, y = self#pos in
        Viz.draw_circle x y cCOLOR_RECOVERED
  end ;;


    (*.................................................................
      Place any augmentations to `infected` here.
    ................................................................ *)

(*....................................................................
Place definitions for any other classes here. In particular, you'll
want to at least implement a `recovered` class for `infected` people
who have recovered from the infection.
....................................................................*)
