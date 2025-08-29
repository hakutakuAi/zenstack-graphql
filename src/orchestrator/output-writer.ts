import { UnifiedGenerationResult } from '@core/types'
import { FileWriter } from '@utils/file-writer'
import { OutputFormat } from '@utils/constants'
import path from 'path'

export class OutputWriter {
	private fileWriter = new FileWriter()

	async write(result: UnifiedGenerationResult, outputPath: string): Promise<string> {
		const finalOutputPath = this.resolveOutputPath(result.outputFormat, outputPath)

		if (result.outputFormat === OutputFormat.TYPE_GRAPHQL) {
			if (!result.code) {
				throw new Error('TypeGraphQL code is required but missing')
			}
			await this.fileWriter.write(result.code, finalOutputPath, 'TypeGraphQL code')
		} else {
			if (!result.sdl) {
				throw new Error('GraphQL SDL is required but missing')
			}
			await this.fileWriter.write(result.sdl, finalOutputPath, 'GraphQL schema')
		}

		return finalOutputPath
	}

	private resolveOutputPath(outputFormat: OutputFormat, basePath: string): string {
		if (outputFormat === OutputFormat.TYPE_GRAPHQL) {
			return path.join(path.dirname(basePath), path.basename(basePath, path.extname(basePath)) + '.ts')
		}
		return basePath
	}
}
