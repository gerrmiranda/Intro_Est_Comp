

for(i in length(q_actuales):1) {
    j = length(p_nuevos)
    while(p_actuales[i]<p_nuevos[j] && j!=1){
        j=j-1
    }
        q_nuevos[j] = q_nuevos[j] + q_actuales[i]
}

