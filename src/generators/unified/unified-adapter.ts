import { DataModel } from '@zenstackhq/sdk/ast'
import { GeneratorContext } from '@core/types'
import { UnifiedGeneratorBase } from './unified-generator-base'
import { BaseGenerator } from '@generators/base-generator'

export class UnifiedGeneratorAdapter extends BaseGenerator {
	private unifiedGenerator: UnifiedGeneratorBase

	constructor(context: GeneratorContext | null, unifiedGenerator: UnifiedGeneratorBase) {
		super(context || ({} as GeneratorContext))
		this.unifiedGenerator = unifiedGenerator
	}

	generate(): string[] {
		return this.unifiedGenerator.generate()
	}
}
