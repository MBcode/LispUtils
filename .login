#set noglob; eval `tset -Q -s`; unset noglob
set term=$TERM
stty decctlq intr "^C" erase "^?" kill "^U"
stty -tabs
cd .
setenv PS2 "`hostname|cut -d. -f2`:"     
alias setp 'set hwd=$cwd:h; set prompt = "@<\! $PS2 /$hwd:t/$cwd:t> "'
setp
setenv CLICOLOR 1
#alias lt0 'ls -lt |head -7|tail -6'
alias lt0 'ls -ot |head -7|tail -6'
#alias ls- 'lsc --color -aFC'
alias ls- 'ls -FC'
#alias cd 'cd \!*; setp; ls -aFC; date;lt0'  
#alias cd 'cd \!*; setp; lsc --color -aFC; date;lt0'  
alias cd 'cd \!*; setp; ls-; date;lt0'  
source ~/.alias
setenv MONTH1 "`cal1m`"
setenv DAY1 "`cal-d`"
alias setp 'set hwd=$cwd:h; set prompt = "λ▶<\! $MONTH1$DAY1$PS2 /$hwd:t/$cwd:t> "'
#alias setp 'set hwd=$cwd:h; set prompt = "→<\! $MONTH1$DAY1$PS2 /$hwd:t/$cwd:t> "'
#alias setp 'set hwd=$cwd:h; set prompt = "▅ <\! $MONTH1$DAY1$PS2 /$hwd:t/$cwd:t> "'
ep2
#echo $DYLD_LIBRARY_PATH
cat ~/.today
echo "=greed=is=a=state=of=fear=I must not fear. Fear is the mind-killer. Fear is the little-death.."
echo "-The future is here; it's just not evenly distributed yet. -William Gibson"
echo "-You can't evaluate a man by logic alone.  -- McCoy, 'I, Mudd', stardate 4513.3"
#echo "It would be illogical to assume that all conditions remain stable.--Spock,"
#echo "  The Enterprise Incident stardate 5027.3" 
echo "It would be illogical2assume that all conditions remain stable.SpockTheEnterpriseIncident*date5027.3" 
echo "-Logic is an organized way of going wrong with confidence; Kettering"
echo "Science is built up with facts, as a house is with stones."
echo " But a collection of facts is no more a science than a heap of stones is a house.  H. Poincare"
echo "-An age is called Dark not because the light fails to shine, but because"
echo "   people refuse to see it.  -- James Michener, Space"
echo "-You cannot achieve the impossible without attempting the absurd."
echo "  Don't worry about what anybody else is going to do. "
echo "   The best way to predict the future is to invent it. - Alan Kay"
echo "What is now proved was once only imagin'd.  -- William Blake"
echo "Zappa: Without deviation from the norm, progress is not possible."
echo "Live as if you were to die tomorrow.*Learn as if you were to live forever."
echo "=[You must be the change you wish to see in the world.]-Mahatma Gandhi"
#xhost +
cy
df2
fortune
/usr/bin/calendar
say "welcome back michael"
#say "happy birthday michael"
uname -a
ncal -w
